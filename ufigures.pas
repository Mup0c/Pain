unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, math, Grids, Spin, UMove, LCLType, Types, FPCanvas;

type

  TFigure = class
    thickness: integer;
    penColor: TColor;
    brushColor: TColor;
    penStyle: TFPPenStyle;
    brushStyle: TFPBrushStyle;
    numOfVertices: integer;
    bounds: TDoubleRect;
    function GetBounds: TDoubleRect; virtual;
    procedure Draw(Canvas: TCanvas); virtual;
    procedure DrawFigure(Canvas: TCanvas); virtual; abstract;
    procedure AddPoint(X, Y: Integer; first: Boolean); virtual; abstract;
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

  TPolygon = class(TTwoPointFigure)
    Vertices: array of TDoublePoint;
    function GetBounds: TDoubleRect; override;
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

implementation

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
begin
  SetLength(Vertices, Length(Vertices) + 1);
  Vertices[High(Vertices)] := ScrToWorld(X, Y);
  If ScrToWorld(X, Y).X < bounds.Left then
    bounds.Left := ScrToWorld(X, Y).X;
  If ScrToWorld(X, Y).Y < bounds.Top then
    bounds.Top := ScrToWorld(X, Y).Y;
  If ScrToWorld(X, Y).X > bounds.Right then
    bounds.Right := ScrToWorld(X, Y).X;
  If ScrToWorld(X, Y).Y > bounds.Bottom then
    bounds.Bottom := ScrToWorld(X, Y).Y;
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
begin
  bnds := imageBounds;
  for i := 0 to numOfVertices - 1 do begin
    If Vertices[i].x < bnds.Left then
      bnds.Left := Vertices[i].X;
    If Vertices[i].Y < bnds.Top then
      bnds.Top := Vertices[i].Y;
    If Vertices[i].x > bnds.Right then
      bnds.Right := Vertices[i].X;
    If Vertices[i].Y > bnds.Bottom then
      bnds.Bottom := Vertices[i].Y;
  end;
  Result := bnds;
end;

end.

