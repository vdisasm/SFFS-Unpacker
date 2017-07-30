unit uFormAddFileNames;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TFormAddFiles = class(TForm)
    Memo1: TMemo;
    ButtonAddFiles: TButton;
    ButtonClear: TButton;
    procedure ButtonAddFilesClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAddFiles: TFormAddFiles;

implementation

uses
  uFormMain;

{$R *.dfm}


procedure TFormAddFiles.ButtonAddFilesClick(Sender: TObject);
begin
  FormMain.DoTryAddFilesFromOpenedList;
end;

procedure TFormAddFiles.ButtonClearClick(Sender: TObject);
begin
  Memo1.Clear;
end;

end.
