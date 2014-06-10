object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'TiffTagReader'
  ClientHeight = 642
  ClientWidth = 965
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    965
    642)
  PixelsPerInch = 120
  TextHeight = 16
  object btnRenameFiles: TButton
    Left = 8
    Top = 0
    Width = 145
    Height = 41
    Caption = 'Load File'
    TabOrder = 0
    OnClick = btnRenameFilesClick
  end
  object TabControl1: TTabControl
    Left = 8
    Top = 47
    Width = 949
    Height = 587
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    Tabs.Strings = (
      '1'
      '2'
      '3')
    TabIndex = 0
    OnChange = TabControl1Change
    DesignSize = (
      949
      587)
    object StringGrid1: TStringGrid
      AlignWithMargins = True
      Left = 3
      Top = 32
      Width = 932
      Height = 546
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColCount = 3
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
      TabOrder = 0
      ColWidths = (
        114
        51
        761)
    end
  end
  object btnSaveIFDItems: TButton
    Left = 159
    Top = 0
    Width = 114
    Height = 41
    Caption = 'Write Tags'
    TabOrder = 2
    OnClick = btnSaveIFDItemsClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.tif'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 576
    Top = 16
  end
end
