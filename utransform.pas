unit UTransform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TDoublePoint = record
      X,Y: double;
    end;
  TDoubleRect = record
     case Integer of
       0: (
           Left: Double;
           Top: Double;
           Right: Double;
           Bottom: Double;
         );
       1: (
           TopLeft: TDoublePoint;
           BottomRight: TDoublePoint;
         );
  end;
  TArrayOfTpoint = array of TPoint;

  function DoublePoint(X,Y: Double): TDoublePoint;
  function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect;
  function DoubleRect(ATopLeft, ABottomRight: TDoublePoint): TDoubleRect;
  procedure SetCanvasPosition(AX, AY: Double);
  procedure CanvasMove(dX, dY: Double);
  function WorldToScr(ADoublePoint: TDoublePoint): TPoint;
  function WorldToScr(AX, AY: Double): TPoint;
  function WorldToScr(ADoubleRect: TDoubleRect): TRect;
  function WorldToScr(AVertices: array of TDoublePoint): TArrayOfTpoint;
  function ScrToWorld(AX, AY: Integer): TDoublePoint;
  procedure SetScale(AScale: Double;  APosX, APosY: Integer);
  procedure AdjustCanvasBounds(ARect: TDoubleRect);
  procedure AdjustImageBounds(AX, AY: Double);

var
  Scale: Double = 1.0;
  CanvasPosition,prevCrds: TDoublePoint;
  imageBounds, canvasBounds: TDoubleRect;

implementation

uses Math;

function DoublePoint(X,Y: Double): TDoublePoint;
  begin
    Result.x := x;
    Result.y := y;
  end;

function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect;
begin
  with Result do begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;

function DoubleRect(ATopLeft, ABottomRight: TDoublePoint): TDoubleRect;
begin
  with Result do begin
    TopLeft := ATopLeft;
    BottomRight := ABottomRight;
  end;
end;

procedure SetScale(AScale: Double;  APosX, APosY: Integer);
var tempCrds: TDoublePoint;
begin
  tempCrds := ScrToWorld(APosX,APosY);
  Scale := EnsureRange(AScale, 0.01, 25);
  CanvasMove(
    tempCrds.X - ScrToWorld(APosX,APosY).X,
    tempCrds.Y - ScrToWorld(APosX,APosY).Y);
end;

procedure AdjustImageBounds(AX, AY: Double);
begin
  If AX < imageBounds.Left then
    imageBounds.Left := AX;
  If AY < imageBounds.Top then
    imageBounds.Top := AY;
  If AX > imageBounds.Right then
    imageBounds.Right := AX;
  If AY > imageBounds.Bottom then
    imageBounds.Bottom := AY;
end;

procedure AdjustCanvasBounds(ARect: TDoubleRect);
begin
  If ARect.Left < imageBounds.Left - 5/scale then
    canvasBounds.Left := ARect.Left
  else
    canvasBounds.Left := imageBounds.Left - 5/scale;
  If ARect.Top < imageBounds.Top - 5/scale then
    canvasBounds.Top := ARect.Top
  else
    canvasBounds.Top := imageBounds.Top - 5/scale;
  If ARect.Right > imageBounds.Right + 5/scale then
    canvasBounds.Right := ARect.Right
  else
    canvasBounds.Right := imageBounds.Right + 5/scale;
  If ARect.Bottom > imageBounds.Bottom + 5/scale then
    canvasBounds.Bottom := ARect.Bottom
  else
    canvasBounds.Bottom := imageBounds.Bottom + 5/scale;
end;

procedure SetCanvasPosition(AX, AY: Double);
begin
  CanvasPosition.X := Scale * AX;
  CanvasPosition.Y := Scale * AY;
end;

procedure CanvasMove(dX, dY: Double);
begin
  CanvasPosition.X += Scale * dX;
  CanvasPosition.Y += Scale * dY;
end;

function WorldToScr(ADoublePoint: TDoublePoint): TPoint;
begin
  with Result do begin
    x := round(Scale * ADoublePoint.X - CanvasPosition.X);
    y := round(Scale * ADoublePoint.Y - CanvasPosition.Y);
  end;
end;

function WorldToScr(AX, AY: Double): TPoint;
begin
  with Result do begin
    x := round(Scale * AX - CanvasPosition.X);
    y := round(Scale * AY - CanvasPosition.Y);
  end;
end;

function WorldToScr(ADoubleRect: TDoubleRect): TRect;
begin
  with Result do begin
    TopLeft := WorldToScr(ADoubleRect.TopLeft);
    BottomRight := WorldToScr(ADoubleRect.BottomRight);
  end;
end;

function WorldToScr(AVertices: array of TDoublePoint): TArrayOfTpoint;
var
  i: Integer;
begin
  SetLength(Result, Length(AVertices));
  for i := 0 to High(Result) do begin
    Result[i] := WorldToScr(AVertices[i]);
  end;
end;

function ScrToWorld(AX, AY: Integer): TDoublePoint;
begin
  with Result do begin
    X := (AX + CanvasPosition.X) / Scale;
    Y := (AY + CanvasPosition.Y) / Scale;
  end;
end;

initialization

CanvasPosition.X := 0;
CanvasPosition.Y := 0;
end.

