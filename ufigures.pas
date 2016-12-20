unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  UTransform, windows, Classes, SysUtils, Graphics, FPCanvas, LCL, math, TypInfo;

type

  StrArr = array of String;

  TFigure = class
    thickness: integer;
    penColor: TColor;
    penStyle: TFPPenStyle;
    bounds: TDoubleRect;
    brushColor: TColor;
    Selected: boolean;
    function GetBounds: TDoubleRect; virtual;
    procedure Move(dX, dY: double); virtual; abstract;
    procedure Draw(Canvas: TCanvas); virtual;
    procedure DrawFigure(Canvas: TCanvas; UsingBrush: boolean); virtual; abstract;
    procedure AddPoint(X, Y: Integer; first: Boolean); virtual; abstract;
    function IsIntersect(ARect: TDoubleRect): boolean; virtual; abstract;
    function Save: StrArr; virtual;
  end;

  TTwoPointFigure = class(TFigure)
    procedure Move(dX, dY: double); override;
    procedure AddPoint(X, Y: Integer; first: Boolean); override;
  end;

  TFilledFigure = class(TTwoPointFigure)
    brushColour: TColor;
    brushStyle: TFPBrushStyle;
    function Save: StrArr; override;
  end;

  TPolyline = class(TFigure)
    vertices: array of TDoublePoint;
    procedure Move(dX, dY: double); override;
    procedure DrawFigure(Canvas: TCanvas; UsingBrush: boolean); override;
    procedure AddPoint(X, Y: Integer; first: Boolean); override;
    function IsIntersect(ARect: TDoubleRect): boolean; override;
    function Save: StrArr; override;
  end;

  TRectangle = class(TFilledFigure)
    procedure DrawFigure(Canvas:TCanvas; UsingBrush: boolean); override;
    function IsIntersect(ARect: TDoubleRect): boolean; override;
  end;

  TEllipse = class(TFilledFigure)
    procedure DrawFigure(Canvas: TCanvas; UsingBrush: boolean); override;
    function IsIntersect(ARect: TDoubleRect): boolean; override;
  end;

  TLine = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas; UsingBrush: boolean); override;
    function IsIntersect(ARect: TDoubleRect): boolean; override;
  end;

  TFrame = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas; UsingBrush: boolean); override;
  end;

  TRoundRect = class(TFilledFigure)
    roundingRadiusX,  roundingRadiusY: integer;
    procedure DrawFigure(Canvas: TCanvas; UsingBrush: boolean); override;
    function IsIntersect(ARect: TDoubleRect): boolean; override;
    function Save: StrArr; override;
  end;

  TPolygon = class(TFilledFigure)
    Vertices: array of TDoublePoint;
    numOfVertices: integer;
    function GetBounds: TDoubleRect; override;
    procedure DrawFigure(Canvas: TCanvas; UsingBrush: boolean); override;
    function IsIntersect(ARect: TDoubleRect): boolean; override;
    function Save: StrArr; override;
  end;

var
  Figures: array of TFigure;

implementation

function TFigure.Save: StrArr;
begin
  SetLength(Result, 5);
  Result[0] := ClassName;
  with Bounds do
  Result[1] := FloatToStr(Top) + ' ' +
               FloatToStr(Left) + ' ' +
               FloatToStr(Bottom) + ' ' +
               FloatToStr(Right);
  Result[2] := IntToStr(thickness);
  Result[3] := GetEnumName(TypeInfo(TFPPenStyle),ord(penStyle));
  Result[4] := ColorToString(penColor);
end;

function TPolyLine.Save: StrArr;
var
  i: integer;
begin
  SetLength(Result, 5);
  Result[0] := ClassName;
  for i := Low(vertices) to High(vertices) do
    Result[1] := Result[1] + ' ' + FloatToStr(vertices[i].X) + ' ' + FloatToStr(vertices[i].Y);
  Result[2] := IntToStr(thickness);
  Result[3] := GetEnumName(TypeInfo(TFPPenStyle),ord(penStyle));
  Result[4] := ColorToString(penColor);
end;

function TFilledFigure.Save: StrArr;
begin
  Inherited;
  Result := Inherited;
  SetLength(Result, Length(Result) + 2);
  Result[High(Result) - 1] := GetEnumName(TypeInfo(TBrushStyle),ord(brushStyle));
  Result[High(Result)] := ColorToString(brushColor);
end;

function TRoundRect.Save: StrArr;
begin
  Inherited;
  Result := Inherited;
  SetLength(Result, Length(Result) + 2);
  Result[High(Result) - 1] := IntToStr(roundingRadiusX);
  Result[High(Result)] := IntToStr(roundingRadiusY);
end;

function TPolygon.Save: StrArr;
begin
Inherited;
  Result := Inherited;
  SetLength(Result, Length(Result) + 1);
  Result[High(Result)] := IntToStr(numOfVertices);
end;                                                   /////////////////////////////////////ON EXIT CLICK спросить сохранить ли


procedure TTwoPointFigure.Move(dX, dY: double);
begin
  bounds.Top -= dY;
  bounds.Bottom -= dY;
  bounds.Left -= dX;
  bounds.Right -= dX;
end;

procedure TPolyline.Move(dX, dY: double);
var i:integer;
begin
  for i:=0 to high(vertices) do begin
    vertices[i].Y -= dY;
    vertices[i].X -= dX;
  end;
  bounds.Top -= dY;
  bounds.Bottom -= dY;
  bounds.Left -= dX;
  bounds.Right -= dX;
end;

procedure TFigure.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := penColor;
  Canvas.Pen.Width := thickness;
  Canvas.Pen.Style := penStyle;
  DrawFigure(Canvas, true);
end;

function AreSegmentsIntersect(A1,A2,B1,B2: TDoublePoint): Boolean;
var
  v1,v2,v3,v4, ax1, ay1, ax2, ay2, bx1, by1, bx2, by2: Double;
 begin
  ax1 := A1.X;
  ay1 := A1.Y;
  ax2 := A2.X;
  ay2 := A2.Y;
  bx1 := B1.X;
  by1 := B1.Y;
  bx2 := B2.X;
  by2 := B2.Y;
  v1:=(bx2-bx1)*(ay1-by1)-(by2-by1)*(ax1-bx1);
  v2:=(bx2-bx1)*(ay2-by1)-(by2-by1)*(ax2-bx1);
  v3:=(ax2-ax1)*(by1-ay1)-(ay2-ay1)*(bx1-ax1);
  v4:=(ax2-ax1)*(by2-ay1)-(ay2-ay1)*(bx2-ax1);
  Result := (v1*v2<0) and (v3*v4<0);
end;

function IsRectIntersectSegment(AFpoint, ASpoint: TDoublePoint; ARect: TDoubleRect): Boolean;
begin
 Result := false;
  with ARect do begin
    if AreSegmentsIntersect(AFpoint,ASpoint, DoublePoint(Left, Bottom), TopLeft) or
       AreSegmentsIntersect(AFpoint,ASpoint, TopLeft, DoublePoint(Right, Top)) or
       AreSegmentsIntersect(AFpoint,ASpoint, DoublePoint(Right, Top), BottomRight) or
       AreSegmentsIntersect(AFpoint,ASpoint, BottomRight, DoublePoint(Left, Bottom)) or
       ((AFpoint.Y <= max(Top,Bottom)) and (AFpoint.Y >= min(Top,Bottom)) and
       (AFpoint.X <= max(Left,Right)) and (AFpoint.X >= min(Left,Right)))
    then result := true;
  end;
end;

function TPolyline.IsIntersect(ARect: TDoubleRect): boolean;
var
  i: integer;
begin
 Result := false;
 for i := 0 to High(vertices)-1 do
  if IsRectIntersectSegment(vertices[i],vertices[i+1],ARect) then Result := true;
end;

function TRectangle.IsIntersect(ARect: TDoubleRect): boolean;
begin
  result := not ((min(bounds.Top,bounds.Bottom) > max(ARect.Top,ARect.Bottom))
              or (max(bounds.Top,bounds.Bottom) < min(ARect.Top,ARect.Bottom))
              or (max(bounds.Left,bounds.Right) < min(ARect.Left,ARect.Right))
              or (min(bounds.Left,bounds.Right) > max(ARect.Left,ARect.Right)));
end;

function TEllipse.IsIntersect(ARect: TDoubleRect): boolean;
var
  Ellipse: HRGN;
begin
  with WorldToScr(bounds) do begin
    Ellipse := CreateEllipticRgn(Left, Top, Right, Bottom);
  end;
  Result := RectInRegion(Ellipse, WorldToScr(ARect));
  DeleteObject(Ellipse);
end;

function TLine.IsIntersect(ARect: TDoubleRect): boolean;
begin
  Result := false;
  if IsRectIntersectSegment(bounds.TopLeft,bounds.BottomRight, ARect) then Result := true;
end;

function TRoundRect.IsIntersect(ARect: TDoubleRect): boolean;
var
  RoundRect: HRGN;
begin
  with WorldToScr(bounds) do begin
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, roundingRadiusX, roundingRadiusY);
  end;
  Result := RectInRegion(RoundRect, WorldToScr(ARect));
  DeleteObject(RoundRect);
end;

function TPolygon.IsIntersect(ARect: TDoubleRect): boolean;
var
  Polygon: HRGN;
  Points: array of TPoint;
begin
  SetLength(Points, numOfVertices);
  Points := WorldToScr(Vertices);
  Polygon := CreatePolygonRgn(Points[0], Length(Points), WINDING);
  Result := RectInRegion(Polygon, WorldToScr(ARect));
  DeleteObject(Polygon);
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

procedure TPolyline.DrawFigure(Canvas: TCanvas; UsingBrush: boolean);
begin
  Canvas.Polyline(WorldToScr(Vertices));
end;

procedure TRectangle.DrawFigure(Canvas: TCanvas; UsingBrush: boolean);
begin
  if UsingBrush then begin
    Canvas.Brush.Color := brushColor;
    Canvas.Brush.Style := brushStyle;
  end;
  Canvas.Rectangle(WorldToScr(bounds));
end;

procedure TEllipse.DrawFigure(Canvas: TCanvas; UsingBrush: boolean);
begin
  if UsingBrush then begin
    Canvas.Brush.Color := brushColor;
    Canvas.Brush.Style := brushStyle;
  end;
  Canvas.Ellipse(WorldToScr(bounds));
end;

procedure TLine.DrawFigure(Canvas: TCanvas; UsingBrush: boolean);
begin
  Canvas.Line(WorldToScr(bounds));
end;

procedure TFrame.DrawFigure(Canvas: TCanvas; UsingBrush: boolean);
begin
  Canvas.Frame(WorldToScr(bounds));
end;

procedure TRoundRect.DrawFigure(Canvas: TCanvas; UsingBrush: boolean);
begin
  if UsingBrush then begin
    Canvas.Brush.Color := brushColor;
    Canvas.Brush.Style := brushStyle;
  end;
  Canvas.RoundRect(WorldToScr(bounds),roundingRadiusX,roundingRadiusY);
end;

procedure TPolygon.DrawFigure(Canvas: TCanvas; UsingBrush: boolean);
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
  if UsingBrush then begin
    Canvas.Brush.Color := brushColor;
    Canvas.Brush.Style := brushStyle;
  end;
  Canvas.Polygon(WorldToScr(Vertices));
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

