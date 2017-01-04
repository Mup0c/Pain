program paint;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMain, UAbout, UTools, UFigures, UTransform
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Paint alpha dark_moon_edition';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainScreen, MainScreen);
  Application.CreateForm(TAbout, About);
  Application.Run;
end.

