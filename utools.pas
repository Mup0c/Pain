unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Buttons, math, Spin,
  UFigures, UTransform, LCLType, FPCanvas;

type

  TFigureClass = class of TFigure;
  TIntegerArray = array of integer;

  TParameter = class
    procedure AddLabel(AName: String; APanel: TPanel); virtual;
    procedure AddEditor(APanel: TPanel; Value: integer); virtual; abstract;
    function GetValue(F: TFigure): integer; virtual; abstract;
  end;

  ArrayOfParameter = array of TParameter;

  TWidthParameter = class(TParameter)
    procedure AddEditor(APanel: TPanel; Value: Integer); override;
    procedure WidthEditChange(Sender: TObject); virtual;
  end;

  TPenStyleParameter = class(TParameter)
    procedure AddEditor(APanel: TPanel; Value: integer);override;
    procedure PenStyleEditChange(Sender: TObject);
    procedure OnDrawLineStyleItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
  end;

  TBrushStyleParameter = class(TParameter)
    procedure AddEditor(APanel: TPanel; Value: integer);override;
    procedure BrushStyleEditChange(Sender: TObject);
    procedure OnDrawBrushStyleItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
  end;

  TVerticesNumberParameter = class(TParameter)
    procedure AddEditor(APanel: TPanel; Value: integer);override;
    procedure NumOfVerticesChange(Sender: TObject);
  end;

  TXRoundingParameter = class(TParameter)
    procedure AddEditor(APanel: TPanel; Value: integer);override;
    procedure RoundingXEditChange(Sender: TObject);
  end;

  TYRoundingParameter = class(TParameter)
    procedure AddEditor(APanel: TPanel; Value: integer);override;
    procedure RoundingYEditChange(Sender: TObject);
  end;

  TTool = class
    FigureClass:TFigureClass;
    Parameters: array of TParameter;
    bmpName: String;
    Figure: TFigure;
    thickness: Integer;
    penColor: TColor;
    penStyle: TFPPenStyle;
    brushColor: TColor;
    ParametersAvailable: boolean;
    function GetFigure: TFigure; virtual;
    procedure SetParams; virtual;
    procedure ToDefaultParams; virtual;
    procedure Init(APanel: TPanel); virtual; abstract;
    procedure MouseDown(X, Y: Integer); virtual; abstract;
    procedure MouseMove(X, Y: Integer); virtual;
    procedure MouseUp(X, Y, AWidth, AHeight: Integer; Shift: TShiftState); virtual;
  end;

  TFilledFigureTool = class(TTool)
    brushColour: TColor;
    brushStyle: TFPBrushStyle;
    procedure SetParams; override;
    procedure ToDefaultParams; override;
  end;

  TPolylineTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TRectangleTool = class(TFilledFigureTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TEllipseTool = class(TFilledFigureTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TLineTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TRoundRectTool = class(TFilledFigureTool)
    roundingRadiusX, roundingRadiusY: integer;
    constructor Create;
    procedure SetParams; override;
    procedure ToDefaultParams; override;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TPolygonTool = class(TFilledFigureTool)
    numOfVertices: integer;
    constructor Create;
    procedure SetParams; override;
    procedure ToDefaultParams; override;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TDragTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TZoomInTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TZoomOutTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TZoomToTool = class(TTool)
    FirstPoint: TDoublePoint;
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseUp(X, Y, AWidth, AHeight: Integer; Shift: TShiftState); override;
  end;

  TSelectorTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseUp(X, Y, AWidth, AHeight: Integer; Shift: TShiftState); override;
  end;

var
  ToolRegistry: array of TTool;
  InvalidateHandler: procedure of Object;
  CurrentTool: TTool;


implementation

procedure TTool.MouseUp(X, Y, AWidth, AHeight: Integer; Shift: TShiftState);
begin
end;

function TTool.GetFigure: TFigure;
begin
  Result := Figure;
end;

procedure UnselectAll;
var i: integer;
begin
  for i := 0 to High(Figures) do
    Figures[i].Selected := false;
end;

procedure TTool.SetParams;
begin
  Figure.thickness := thickness;
  Figure.penColor := penColor;
  Figure.penStyle := penStyle;
end;

procedure TFilledFigureTool.SetParams;
begin
  Inherited;
  (Figure as TFilledFigure).brushColor := brushColor;
  (Figure as TFilledFigure).brushStyle := brushStyle;
end;

procedure TRoundRectTool.SetParams;
begin
  Inherited;
  (Figure as TRoundRect).roundingRadiusX := roundingRadiusX;
  (Figure as TRoundRect).roundingRadiusY := roundingRadiusY;
end;

procedure TPolygonTool.SetParams;
begin
  Inherited;
  (Figure as TPolygon).numOfVertices := numOfVertices;
end;

procedure TTool.ToDefaultParams;
begin
  thickness := 1;
  penStyle := psSolid;
end;

procedure TFilledFigureTool.ToDefaultParams;
begin
  Inherited;
  brushStyle := bsSolid;
end;

procedure TRoundRectTool.ToDefaultParams;
begin
  Inherited;
  roundingRadiusX := 10;
  roundingRadiusY := 10;
end;

procedure TPolygonTool.ToDefaultParams;
begin
  Inherited;
  numOfVertices := 3;
end;

procedure TTool.MouseMove(X, Y: Integer);
begin
    Figure.AddPoint(X, Y, false)
end;

procedure TParameter.AddLabel(AName: String; APanel: TPanel);
begin
  With TLabel.Create(APanel) do begin
    Parent := APanel;
    AutoSize := false;
    Left := 2;
    Top := 2;
    Align := altop;
    BorderSpacing.Around:= 5;
    Font.Size := 10;
    Caption := AName;
  end;
end;

procedure TWidthParameter.WidthEditChange(Sender: TObject);
begin
  CurrentTool.thickness := (Sender as TSpinEdit).Value
end;

procedure TWidthParameter.AddEditor(APanel: TPanel; Value: Integer);
var WidthEdit: TSpinEdit;
begin
  WidthEdit := TSpinEdit.Create(APanel);
  With WidthEdit do begin
    Parent := APanel;
    Left := 2;
    Top := 2;
    Align := altop;
    MinValue := 1;
    MaxValue := 100;
    BorderSpacing.Around:= 5;
    OnChange := @WidthEditChange;
  end;
  AddLabel('Thickness:', APanel);
end;

procedure TXRoundingParameter.RoundingXEditChange(Sender: TObject);
begin
  (CurrentTool as TRoundRectTool).roundingRadiusX := (Sender as TSpinEdit).Value;
end;

Procedure TXRoundingParameter.AddEditor(APanel: TPanel; Value: Integer);
var
  RoundingEdit: TSpinEdit;
begin
  RoundingEdit := TSpinEdit.Create(APanel);
  With RoundingEdit do begin
    Parent := APanel;
    Left := 2;
    Top := 2;
    Align := altop;
    MinValue := 0;
    MaxValue := 1000;
    Value := 10;
    BorderSpacing.Around:= 5;
    OnChange := @RoundingXEditChange;
  end;
  AddLabel('Rounding X:', APanel);
end;

procedure TYRoundingParameter.RoundingYEditChange(Sender: TObject);
begin
  (CurrentTool as TRoundRectTool).roundingRadiusY := (Sender as TSpinEdit).Value;
end;

Procedure TYRoundingParameter.AddEditor(APanel: TPanel; Value: Integer);
var
  RoundingEdit: TSpinEdit;
begin
  RoundingEdit := TSpinEdit.Create(APanel);
  With RoundingEdit do begin
    Parent := APanel;
    Left := 2;
    Top := 2;
    Align := altop;
    MinValue := 0;
    MaxValue := 1000;
    Value := 10;
    BorderSpacing.Around:= 5;
    OnChange := @RoundingYEditChange;
  end;
  AddLabel('Rounding Y:', APanel);
end;

procedure TPenStyleParameter.PenStyleEditChange(Sender: TObject);
begin
  CurrentTool.penStyle := TFPPenStyle((sender as TComboBox).ItemIndex);
end;

procedure TPenstyleParameter.OnDrawLineStyleItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas, ARect do begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Pen.Style := psClear;
    Pen.Color := clWhite;
    Top += 3;
    Left += 3;
    Right -= 3;
    Bottom -= 3;
    Rectangle(ARect);
    Pen.Style := TFPPenStyle(Index);
    Pen.Width := 3;
    Pen.Color := clBlack;
    Top += 6;
    Bottom := ARect.Top;
    Line(ARect);
  end;
end;

procedure TPenstyleParameter.AddEditor(APanel: TPanel; Value: Integer);
var
  PenStyleEdit: TComboBox;
  i:integer;
begin
  PenStyleEdit := TComboBox.Create(APanel);
  With PenStyleEdit do begin
    Parent := APanel;
    Left := 2;
    Top := 2;
    Align := altop;
    for i := 0 to 4 do Items.Add('');
    Style := csOwnerDrawFixed;
    ReadOnly := True;
    ItemIndex := 0;
    AutoComplete := False;
    Font.Bold := True;
    Font.Size := 12;
    BorderSpacing.Around:= 5;
    OnChange := @PenStyleEditChange;
    OnDrawItem := @OnDrawLineStyleItem;
  end;
  AddLabel('Pen Style:', APanel);
end;

procedure TBrushStyleParameter.BrushStyleEditChange(Sender: TObject);
begin
  (CurrentTool as TFilledFigureTool).brushStyle := TFPBrushStyle((sender as TComboBox).ItemIndex);
end;

procedure TBrushStyleParameter.OnDrawBrushStyleItem(
  Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas, ARect do begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Top += 2;
    Left += 2;
    Right -= 2;
    Bottom -= 2;
    Rectangle(ARect);
    if TFPBrushStyle(Index) = bsClear then begin
      Brush.Color := clWhite;
      Brush.Style := bsSolid;
    end
    else begin
      Brush.Style := TFPBrushStyle(Index);
      Brush.Color := clBlack;
    end;
    Pen.Color := clBlack;
    Rectangle(ARect);
  end;
end;

procedure TBrushStyleParameter.AddEditor(APanel: TPanel; Value: Integer);
var
  BrushStyleEdit: TComboBox;
  i: integer;
begin
  BrushStyleEdit := TComboBox.Create(APanel);
  With BrushStyleEdit do begin
    Parent := APanel;
    Left := 2;
    Top := 2;
    Align := altop;
    for i := 0 to 7 do Items.Add('');
    Font.Size := 10;
    ItemIndex := 0;
    BorderSpacing.Around:= 5;
    ReadOnly := True;
    Style := csOwnerDrawFixed;
    OnChange := @BrushStyleEditChange;
    OnDrawItem := @OnDrawBrushStyleItem;
  end;
  AddLabel('Brush Style:', APanel);
end;

procedure TVerticesNumberParameter.AddEditor(APanel: TPanel; Value: Integer);
var
  NumOfVerticesEdit: TSpinEdit;
  /////
begin
  NumOfVerticesEdit := TSpinEdit.Create(APanel);
  With NumOfVerticesEdit do begin
    Parent := APanel;
    Left := 2;
    Top := 2;
    Align := altop;
    MinValue := 3;
    MaxValue := 16;
    BorderSpacing.Around:= 5;
    OnChange := @NumOfVerticesChange;
  end;
  AddLabel('Vertices:', APanel);
end;

procedure TVerticesNumberParameter.NumOfVerticesChange(Sender: TObject);
begin
  //NumOfVertices := NumOfVerticesEdit.Value;
  (CurrentTool as TPolygonTool).NumOfVertices := (Sender as TSpinEdit).Value;
end;

procedure TPolylineTool.MouseDown(X, Y: Integer);
begin
  Figure := TPolyline.Create;
  SetParams;
  with Figure.bounds do begin
    Top := ScrToWorld(X, Y).Y;
    Left := ScrToWorld(X, Y).X;
    Bottom := ScrToWorld(X, Y).Y;
    Right := ScrToWorld(X, Y).X;
  end;
  Figure.AddPoint(X, Y, true);
end;

constructor TPolylineTool.Create;
begin
  Inherited;
  bmpName := 'icons/Polyline.bmp';
end;

procedure TPolylineTool.Init(APanel: TPanel);
var i:integer;
begin
  UnselectAll;
  ParametersAvailable := true;
  SetLength(Parameters,2);
  Parameters[0] := TWidthParameter.Create;
  Parameters[1] := TPenStyleParameter.Create;
  for i := High(Parameters) downto 0 do
    Parameters[i].AddEditor(APanel, 0);
  ToDefaultParams;
end;

procedure TRectangleTool.MouseDown(X, Y: Integer);
begin
  Figure := TRectangle.Create;
  SetParams;
  Figure.AddPoint(X, Y, true);
end;

constructor TRectangleTool.Create;
begin
  Inherited;
  bmpName := 'icons/Rectangle.bmp';
end;

procedure TRectangleTool.Init(APanel: TPanel);
var i:integer;
begin
  UnselectAll;
  ParametersAvailable := true;
  SetLength(Parameters,3);
  Parameters[0] := TWidthParameter.Create;
  Parameters[1] := TPenStyleParameter.Create;
  Parameters[2] := TBrushStyleParameter.Create;
  for i := High(Parameters) downto 0 do
    Parameters[i].AddEditor(APanel, 0);
  ToDefaultParams;
end;

procedure TSelectorTool.MouseDown(X, Y: Integer);
begin
  Figure := TFrame.Create;
  Figure.AddPoint(X, Y, true);
end;

constructor TSelectorTool.Create;
begin
  Inherited;
  bmpName := 'icons/Selector.bmp';
end;

procedure TSelectorTool.MouseUp(X, Y, AWidth, AHeight: Integer; Shift: TShiftState);
var i: Integer;
  boundsWithWidth:TDoubleRect;
  th: Double;
begin
  with Figure.bounds do begin
    if (Left = Right) or (Top = Bottom) then begin
      Top -= 1/scale;
      Left -= 1/scale;
      Bottom += 1/scale;
      Right += 1/scale;
    end;
  end;
  if not (ssCtrl in Shift) then
    UnselectAll;
  for i := High(Figures) downto 0 do begin
    th := Figures[i].thickness/scale;
    with figure.bounds do begin
      if Left < Right then
        boundsWithWidth := DoubleRect(Left - (th/2), Top, Right + (th/2), Bottom)
      else
        boundsWithWidth := DoubleRect(Left + (th/2), Top, Right - (th/2), Bottom);
      if Top < Bottom then
        boundsWithWidth := DoubleRect(boundsWithWidth.Left, Top - (th/2),
                                      boundsWithWidth.Right, Bottom + (th/2))
      else
        boundsWithWidth := DoubleRect(boundsWithWidth.Left, Top + (th/2),
                                      boundsWithWidth.Right, Bottom - (th/2));
    end;
    if Figures[i].IsIntersect(boundsWithWidth) then begin
      if ssCtrl in Shift then
        Figures[i].Selected := not Figures[i].Selected
      else
        Figures[i].Selected := true;
      if (abs(Figure.bounds.Left - Figure.bounds.Right) < 4/scale) and
         (abs(Figure.bounds.Top - Figure.bounds.Bottom) < 4/scale)
      then
        break;
    end;
  end;
  Figure := nil;
end;

procedure TSelectorTool.Init(APanel: TPanel);
begin
  ParametersAvailable := false;
end;

procedure TEllipseTool.MouseDown(X, Y: Integer);
begin
  Figure := TEllipse.Create;
  SetParams;
  Figure.AddPoint(X, Y, true);
end;

constructor TEllipseTool.Create;
begin
  Inherited;
  bmpName := 'icons/Ellipse.bmp';
end;

procedure TEllipseTool.Init(APanel: TPanel);
var i :integer;
begin
  UnselectAll;
  ParametersAvailable := true;
  SetLength(Parameters,3);
  Parameters[0] := TWidthParameter.Create;
  Parameters[1] := TPenStyleParameter.Create;
  Parameters[2] := TBrushStyleParameter.Create;
  for i := High(Parameters) downto 0 do
    Parameters[i].AddEditor(APanel, 0);
  ToDefaultParams;
end;

procedure TRoundRectTool.MouseDown(X, Y: Integer);
begin
  Figure := TRoundRect.Create;
  SetParams;
  Figure.AddPoint(X, Y, true);
end;

constructor TRoundRectTool.Create;
begin
  Inherited;
  bmpName := 'icons/RoundRect.bmp';
end;

procedure TRoundRectTool.Init(APanel: TPanel);
var i: integer;
begin
  UnselectAll;
  ParametersAvailable := true;
  SetLength(Parameters,5);
  Parameters[0] := TWidthParameter.Create;
  Parameters[1] := TPenStyleParameter.Create;
  Parameters[2] := TBrushStyleParameter.Create;
  Parameters[3] := TXRoundingParameter.Create;
  Parameters[4] := TYRoundingParameter.Create;
  for i := High(Parameters) downto 0 do
    Parameters[i].AddEditor(APanel, 0);
  ToDefaultParams;
end;

procedure TLineTool.MouseDown(X, Y: Integer);
begin
  Figure := TLine.Create;
  SetParams;
  Figure.AddPoint(X, Y, true);
end;

constructor TLineTool.Create;
begin
  Inherited;
  bmpName := 'icons/Line.bmp';
end;

procedure TLineTool.Init(APanel: TPanel);
var i:integer;
begin
  UnselectAll;
  ParametersAvailable := true;
  SetLength(Parameters,2);
  Parameters[0] := TWidthParameter.Create;
  Parameters[1] := TPenStyleParameter.Create;
  for i := High(Parameters) downto 0 do
    Parameters[i].AddEditor(APanel, 0);
  ToDefaultParams;
end;

procedure TPolygonTool.MouseDown(X, Y: Integer);
begin
  Figure := TPolygon.Create;
  SetParams;
  Figure.AddPoint(X, Y, true);
end;

constructor TPolygonTool.Create;
begin
  Inherited;
  bmpName := 'icons/Polygon.bmp';
end;

procedure TPolygonTool.Init(APanel: TPanel);
var i:integer;
begin
  UnselectAll;
  ParametersAvailable := true;
  SetLength(Parameters,4);
  Parameters[0] := TWidthParameter.Create;
  Parameters[1] := TPenStyleParameter.Create;
  Parameters[2] := TBrushStyleParameter.Create;
  Parameters[3] := TVerticesNumberParameter.Create;
  for i := High(Parameters) downto 0 do
    Parameters[i].AddEditor(APanel, 0);
  ToDefaultParams;
end;

procedure TDragTool.MouseDown(X, Y: Integer);
begin
  prevCrds := ScrToWorld(X,Y);
end;

procedure TDragTool.MouseMove(X, Y: Integer);
var i,j: Integer;
  dX,dY:Double;
  anySelected: boolean;
begin
  anySelected := False;
  dX := prevCrds.X - ScrToWorld(X,Y).X;
  dY := prevCrds.Y - ScrToWorld(X,Y).Y;
  for i := 0 to High(Figures) do begin
    if Figures[i].Selected then begin
      Figures[i].Move(dX,dY);
      anySelected:= true;
    end;
  end;
  if not anySelected then
    CanvasMove(dX, dY)
  else
    prevCrds := ScrToWorld(X,Y);
end;

constructor TDragTool.Create;
begin
  Inherited;
  bmpName := 'icons/Drag.bmp';
end;

procedure TDragTool.Init(APanel: TPanel);
begin
  ParametersAvailable := False;
end;

procedure TZoomInTool.MouseDown(X, Y: Integer);
begin
  SetScale(Scale * 2, X, Y)
end;

procedure TZoomInTool.MouseMove(X, Y: Integer);
begin
end;

constructor TZoomInTool.Create;
begin
  Inherited;
  bmpName := 'icons/ZoomIn.bmp';
end;

procedure TZoomInTool.Init(APanel: TPanel);
begin
  ParametersAvailable := False;
end;

procedure TZoomOutTool.MouseDown(X, Y: Integer);
begin
  SetScale(Scale / 2, X, Y)
end;

procedure TZoomOutTool.MouseMove(X, Y: Integer);
begin
end;

constructor TZoomOutTool.Create;
begin
  Inherited;
  bmpName := 'icons/ZoomOut.bmp';
end;

procedure TZoomOutTool.Init(APanel: TPanel);
begin
  ParametersAvailable := False;
end;

procedure TZoomToTool.MouseDown(X, Y: Integer);
begin
  Figure := TFrame.Create;
  Figure.AddPoint(X, Y, true);
  FirstPoint := ScrToWorld(X, Y);
end;

procedure TZoomToTool.MouseMove(X, Y: Integer);
begin
  Figure.AddPoint(X, Y, false);
end;

procedure TZoomToTool.MouseUp(X, Y, AWidth, AHeight: Integer; Shift: TShiftState);
var Bottom, Top, Right, Left: Double;
begin
  Top := Figure.bounds.Top;
  Bottom := Figure.bounds.Bottom;
  Right := Figure.bounds.Right;
  Left := Figure.bounds.Left;
  If (Top <> Bottom) and (Right <> Left) then begin
    Scale := min(AHeight / abs(Top - Bottom), AWidth / abs(Left - Right));
    SetCanvasPosition(min(Left, Right), min(Top, Bottom));
  end;
  Figure := nil;
end;

constructor TZoomToTool.Create;
begin
  Inherited;
  bmpName := 'icons/ZoomTo.bmp';
end;

procedure TZoomToTool.Init(APanel: TPanel);
begin
  ParametersAvailable := False;
end;

procedure RegisterTool(Tool: TTool);
begin
  SetLength(ToolRegistry, Length(ToolRegistry) + 1);
  ToolRegistry[High(ToolRegistry)] := Tool;
end;

initialization

RegisterTool(TDragTool.Create);
RegisterTool(TSelectorTool.Create);
RegisterTool(TPolyLineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TRoundRectTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);
RegisterTool(TPolygonTool.Create);
RegisterTool(TZoomInTool.Create);
RegisterTool(TZoomOutTool.Create);
RegisterTool(TZoomToTool.Create);


end.

