unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ComCtrls, Tiff;

type
  TfrmMain = class(TForm)
    OpenDialog: TOpenDialog;
    btnRenameFiles: TButton;
    TabControl1: TTabControl;
    StringGrid1: TStringGrid;
    btnSaveIFDItems: TButton;
    procedure btnRenameFilesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure btnSaveIFDItemsClick(Sender: TObject);
    procedure btnLabelMacroClick(Sender: TObject);
  private
    { Private declarations }
    tiffimage:TTIFFInfo;
    filename:string;
    procedure RefreshTab;
    procedure SaveSubImage(idx:integer;srcfile,destfile:string);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses ShellApi;
{$R *.dfm}

procedure TfrmMain.btnLabelMacroClick(Sender: TObject);
var
  i:integer;
  subimage:TTiffSubInfoImage;
  IFDItem:TIFDItem;
begin
  //
  for I := 0 to tiffimage.SubImages.Count - 1 do
  begin
    subimage:=TTiffSubInfoImage(tiffImage.SubImages[i]);
    IFDItem:=subimage.IFDItems.FindItem(ttt_ImageDescription);
    if IFDItem.StrValue='Macro' then
      SaveSubImage(I,filename,ChangeFileExt(filename,'.macro.jpg'));
    if IFDItem.StrValue='Label' then
      SaveSubImage(I,filename,ChangeFileExt(filename,'.label.jpg'));
  end;
end;

procedure TfrmMain.btnRenameFilesClick(Sender: TObject);
var
  J:integer;
  filestream:TFileStream;
begin
  // Select file(s)

  if OpenDialog.Execute then
  begin
    filename:=OpenDialog.FileName;
    filestream:=TFileStream.Create(filename,fmOpenRead);
    tiffimage.LoadFromStream(filestream);
    TabControl1.Tabs.Clear;
    for J := 1 to tiffimage.SubImages.Count do
      TabControl1.Tabs.Add(IntToStr(J));
    TabControl1.TabIndex:=0;
    RefreshTab;
    filestream.Free;
  end;

end;

procedure TfrmMain.btnSaveIFDItemsClick(Sender: TObject);
var
  subimage:TTiffSubInfoImage;
  I: Integer;
  line:string;
  strings:TStringList;
begin
  strings:=TStringList.Create;
  subimage:=TTiffSubInfoImage(tiffImage.SubImages[TabControl1.TabIndex]);
  for I := 0 to subimage.IFDItems.Count - 1 do
  begin
    strings.Add(subimage.IFDItems[I].DisplayNameValues);
  end;
  strings.SaveToFile(ChangeFileExt(filename,'.properties.txt'));
  strings.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  debug:=False;
  StringGrid1.Cells[0,0]:='Name';
  StringGrid1.Cells[1,0]:='Length';
  StringGrid1.Cells[2,0]:='Value';
  tiffimage:=TTIFFInfo.Create;
end;

procedure TfrmMain.RefreshTab;
var
  subimage:TTiffSubInfoImage;
  I: Integer;
  line:string;
begin
  subimage:=TTiffSubInfoImage(tiffImage.SubImages[TabControl1.TabIndex]);
  StringGrid1.RowCount:=subimage.IFDItems.Count+2;
  for I := 0 to subimage.IFDItems.Count - 1 do
  begin
    line:=subimage.IFDItems[I].DisplayNameValues;
    StringGrid1.Cells[0,I+1]:=Trim(Copy(line,1,36));
    StringGrid1.Cells[1,I+1]:=Copy(line,37,6);
    StringGrid1.Cells[2,I+1]:=Copy(line,43,length(line));
  end;
  StringGrid1.Cells[0,subimage.IFDItems.Count+1]:='Range';
  StringGrid1.Cells[1,subimage.IFDItems.Count+1]:=intToSTr(subimage.Lastpos-subimage.StartPos);
  StringGrid1.Cells[2,subimage.IFDItems.Count+1]:=IntToStr(subimage.Startpos)+' '+IntToStr(subimage.Lastpos);
end;

procedure TfrmMain.SaveSubImage(idx: integer;srcfile,destfile:string);
begin
  ShellExecute(Handle,'Open',pchar('i_view32.exe'),pchar('"'+srcfile+'" /page='+intToStr(idx+1)+' /convert="'+destfile+'"'),'',sw_Hide);
end;

procedure TfrmMain.TabControl1Change(Sender: TObject);
begin
  RefreshTab;
end;

end.
