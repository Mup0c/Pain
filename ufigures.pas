unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, math, Grids, Spin, UMove, LCLType, Types, FPCanvas;

type

  TFigure = class
    thickness: integer;
    penColor: TColor;                                          ////////
    brushColor: TColor;
    penStyle: TFPPenStyle;
    brushStyle: TFPBrushStyle;
    numOfVertices: integer;
    roundingRadiusX,  roundingRadiusY: integer;
    bounds: TDoubleRect;
    Selected: boolean;
    function GetBounds: TDoubleRect; virtual;
    procedure Draw(Canvas: TCanvas); virtual;
    procedure DrawFigure(Canvas: TCanvas); virtual; abstract;
    procedure AddPoint(X, Y: Integer; first: Boolean); virtual; abstract;
    function IsIntersect(ARect: TDoubleRect): boolean;
  end;

  TTwoPointFigure = class(TFigure)
    procedure AddPoint(X, Y: Integer; first: Boolean); override;
  end;

  TPolyline = class(TFigure)
    vertices: array of TDoublePoint;
    procedure DrawFigure(Canvas: TCanvas); override;
    procedure AddPoint(X, Y: Integer; first: Boolean); override;
  end;

  TRectangle = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TEllipse = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TLine = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TFrame = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TRoundRect = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TPolygon = class(TTwoPointFigure)
    Vertices: array of TDoublePoint;
    function GetBounds: TDoubleRect; override;
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

var
  Figures: array of TFigure;

implementation

function TFigure.IsIntersect(ARect: TDoubleRect): boolean;
begin
  result := not ((min(bounds.Top,bounds.Bottom) > max(ARect.Top,ARect.Bottom)) or (max(bounds.Top,bounds.Bottom) < min(ARect.Top,ARect.Bottom))
              or (max(bounds.Left,bounds.Right) < min(ARect.Left,ARect.Right)) or (min(bounds.Left,bounds.Right) > max(ARect.Left,ARect.Right)));
end;

procedure TFigure.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := penColor;
  Canvas.Brush.Color := brushColor;
  Canvas.Pen.Width := thickness;
  Canvas.Pen.Style := penStyle;
  Canvas.Brush.Style := brushStyle;
  DrawFigure(Canvas);
end;

function TFigure.Getbounds: TDoubleRect;
begin
  Result := bounds;
end;

procedure TPolyline.AddPoint(X, Y: Integer; first: Boolean);
var
  wrldx, wrldy: double;
begin
  wrldx:= ScrToWorld(X, Y).X;
  wrldy:= ScrToWorld(X, Y).Y;
  SetLength(Vertices, Length(Vertices) + 1);
  Vertices[High(Vertices)] := ScrToWorld(X, Y);
  If wrldx < bounds.Left then
    bounds.Left := wrldx;
  If wrldy < bounds.Top then
    bounds.Top := wrldy;
  If wrldx > bounds.Right then
    bounds.Right := wrldx;
  If wrldy > bounds.Bottom then
    bounds.Bottom := wrldy;
end;

procedure TTwoPointFigure.AddPoint(X, Y: Integer; first: Boolean);
begin
  if first then
    bounds := DoubleRect(ScrToWorld(X, Y), ScrToWorld(X, Y))
  else
    bounds.BottomRight := ScrToWorld(X, Y)
end;

procedure TPolyline.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Polyline(WorldToScrCrds(Vertices));
end;

procedure TRectangle.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Rectangle(WorldToScrCrds(bounds));
end;

procedure TEllipse.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Ellipse(WorldToScrCrds(bounds));
end;

procedure TLine.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Line(WorldToScrCrds(bounds));
end;

procedure TFrame.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Frame(WorldToScrCrds(bounds));
end;

procedure TRoundRect.DrawFigure(Canvas: TCanvas);
begin
  Canvas.RoundRect(WorldToScrCrds(bounds),roundingRadiusX,roundingRadiusY);
end;

procedure TPolygon.DrawFigure(Canvas: TCanvas);
var
  i: Integer;
  Center: TDoublePoint;
  Radius: Double;
begin
  Center.X := (Bounds.Left + Bounds.Right) / 2;
  Center.Y := (Bounds.Top + Bounds.Bottom ) / 2;
  Radius := Min(abs(Bounds.Right - Center.X), abs(Bounds.Bottom - Center.Y));
  SetLength(Vertices, numOfVertices);
  for i := 0 to numOfVertices - 1 do begin
    Vertices[i].x := Center.X + (Radius*sin(i * 2 * pi / numOfVertices));
    Vertices[i].y := Center.Y + (Radius*cos(i * 2 * pi / numOfVertices));
  end;
  Canvas.Polygon(WorldToScrCrds(Vertices));
end;

function TPolygon.Getbounds: TDoubleRect;
var
  bnds: TDoubleRect;
  i: Integer;
  x, y: double;
begin
  x := Vertices[0].x;
  y := Vertices[0].y;
  bnds := DoubleRect(x,y,x,y);
  for i := 0 to numOfVertices - 1 do begin
    x := Vertices[i].x;
    y := Vertices[i].y;
    If x < bnds.Left then
      bnds.Left := x;
    If y < bnds.Top then
      bnds.Top := y;
    If x > bnds.Right then
      bnds.Right := x;
    If y > bnds.Bottom then
      bnds.Bottom := y;
  end;
  Result := bnds;
end;

end.

