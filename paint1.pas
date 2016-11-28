unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, ComCtrls, about1, Types;

type

  { TMainScreen }

  TMainScreen = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuExit: TMenuItem;
    MenuAbout: TMenuItem;
    MenuClear: TMenuItem;
    PaintField: TPaintBox;
    LWidth: TTrackBar;
    procedure MenuExitClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuClearClick(Sender: TObject);
    procedure PaintFieldMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintFieldMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintFieldMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintFieldPaint(Sender: TObject);
    procedure LWidthChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  type Line = record
     pts:array of Tpoint;
     width:integer;
     colour:TColor;
  end;

var
  MainScreen: TMainScreen;
  mouse_down, width_changed: boolean;
  lines: array of Line;

implementation

{$R *.lfm}

{ TMainScreen }

procedure TMainScreen.MenuExitClick(Sender: TObject);
begin
   Application.Terminate;
end;

procedure TMainScreen.MenuAboutClick(Sender: TObject);
begin
   About.show;
end;

procedure TMainScreen.MenuClearClick(Sender: TObject);
begin
   lines := nil;
   PaintField.Canvas.Clear;
end;

procedure TMainScreen.PaintFieldMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mouse_down := true;
   PaintField.Canvas.MoveTo(point(x,y));
   PaintField.Canvas.Pen.Color:= clHighlight;
   if width_changed = false then begin
     SetLength(lines, Length(lines) + 1);
     lines[High(lines)].width := LWidth.Position;
     PaintField.Canvas.Pen.Width := lines[High(lines)].width;
   end;
   SetLength(lines[high(lines)].pts, Length(lines[high(lines)].pts) + 1);
   lines[High(lines)].pts[High(lines[High(lines)].pts)] := point (x,y);
end;

procedure TMainScreen.PaintFieldMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
   if mouse_down then begin
    PaintField.Canvas.LineTo(point(x,y));
    SetLength(lines[high(lines)].pts, Length(lines[high(lines)].pts) + 1);
    lines[High(lines)].pts[High(lines[High(lines)].pts)] := point (x,y);
   end;
end;

procedure TMainScreen.PaintFieldMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    mouse_down := false;
    width_changed := false;
end;

procedure TMainScreen.PaintFieldPaint(Sender: TObject);
var i:integer;
begin
  for i := low(lines) to high(lines) do begin
    PaintField.Canvas.Pen.Width := lines[i].width;
    PaintField.Canvas.Polyline(lines[i].pts);
    lines[High(lines)].width := LWidth.Position;
    PaintField.Canvas.Pen.Width := lines[High(lines)].width;
  end;
end;

procedure TMainScreen.LWidthChange(Sender: TObject);
begin
  width_changed := true;
  setlength(lines, length(lines) + 1);
  lines[High(lines)].width := LWidth.Position;
  PaintField.Canvas.Pen.Width := lines[High(lines)].width;
end;

end.

