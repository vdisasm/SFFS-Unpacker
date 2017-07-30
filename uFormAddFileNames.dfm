object FormAddFiles: TFormAddFiles
  Left = 0
  Top = 0
  Caption = 'Add file names'
  ClientHeight = 267
  ClientWidth = 642
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    642
    267)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 642
    Height = 228
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object ButtonAddFiles: TButton
    Left = 8
    Top = 234
    Width = 113
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add'
    TabOrder = 1
    OnClick = ButtonAddFilesClick
  end
  object ButtonClear: TButton
    Left = 127
    Top = 234
    Width = 98
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 2
    OnClick = ButtonClearClick
  end
end
