{$IFNDEF DEBUG}
  {$IFDEF WIN32}
    {$E '32.exe'}
  {$ELSE}
    {$E '64.exe'}
  {$ENDIF}
{$ENDIF}
program sffsunpacker;

{$IF CompilerVersion >= 21}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

uses
  Vcl.Forms,
  uFormMain in 'uFormMain.pas' {FormMain},
  SFFS.Unpacker.Project in 'SFFS.Unpacker.Project.pas',
  SFFS.AppKey in 'SFFS.AppKey.pas',
  uFormAddFileNames in 'uFormAddFileNames.pas' {FormAddFiles},
  SFFS.Unpacker.Thread in 'SFFS.Unpacker.Thread.pas',
  SFFS.Helpers in 'SFFS.Helpers.pas',
  SFFS.Types in 'SFFS.Types.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAddFiles, FormAddFiles);
  Application.Run;
end.
