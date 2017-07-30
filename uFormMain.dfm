object FormMain: TFormMain
  Left = 0
  Top = 0
  ActiveControl = ListViewFiles
  Caption = 'SFFS Unpacker'
  ClientHeight = 541
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListViewFiles: TListView
    Left = 0
    Top = 0
    Width = 784
    Height = 522
    Align = alClient
    BorderStyle = bsNone
    Color = clWhite
    Columns = <
      item
        AutoSize = True
        Caption = 'file name hash'
        MaxWidth = 300
      end
      item
        AutoSize = True
        Caption = '#'
        MaxWidth = 50
      end
      item
        Caption = 'file name'
        Width = 463
      end>
    DoubleBuffered = True
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    GridLines = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListViewFilesChange
    OnData = ListViewFilesData
    OnDblClick = ListViewFilesDblClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 522
    Width = 784
    Height = 19
    Panels = <
      item
        Width = 75
      end
      item
        Width = 125
      end
      item
        Width = 50
      end>
  end
  object MainMenu1: TMainMenu
    Left = 408
    Top = 216
    object MenuFile: TMenuItem
      Caption = 'File'
      object MenuSaveProject: TMenuItem
        Caption = 'Save Project'
        ShortCut = 16467
        OnClick = MenuSaveProjectClick
      end
      object MenuCloseProject: TMenuItem
        Caption = 'Close Project'
        ShortCut = 16471
        OnClick = MenuCloseProjectClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Addfilenames1: TMenuItem
        Caption = 'Add file names...'
        ShortCut = 116
        OnClick = Addfilenames1Click
      end
      object MenuExportListAsText: TMenuItem
        Caption = 'Export list as text'
        OnClick = MenuExportListAsTextClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MenuExtractAll: TMenuItem
        Caption = 'Extract All'
        ShortCut = 120
        OnClick = MenuExtractAllClick
      end
      object AbortExtraction1: TMenuItem
        Caption = 'Abort Extraction'
        ShortCut = 27
        OnClick = AbortExtraction1Click
      end
    end
  end
end
