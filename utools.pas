unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Buttons, math, Spin,
  UFigures, UMove, LCLType, FPCanvas;

type

  TParameter = class
  end;

  TTool = class
    WidthEdit: TSpinEdit;
    RoundingXEdit: TSpinEdit;
    RoundingYEdit: TSpinEdit;
    parameters: array of TParameter;
    bmpName: String;
    Figure: TFigure;
    thickness: Integer;
    penColor: TColor;                                           ///////// раскидать в насделников
    brushColor: TColor;
    penStyle: TFPPenStyle;
    brushStyle: TFPBrushStyle;
    numOfVertices: integer;
    ParametersAvailable: boolean;
    PenStyleEdit: TComboBox;
    BrushStyleEdit: TComboBox;
    NumOfVerticesEdit: TSpinEdit;
    roundingRadiusX, roundingRadiusY: integer;
    function GetFigure: TFigure; virtual;
    procedure SetParams; virtual;
    procedure ToDefaultParams; virtual;
    procedure AddWidthEdit(APanel: TPanel); virtual;
    function AddRoundingEdit(APanel: TPanel; AEditorChange: TNotifyEvent): TSpinEdit; virtual;
    procedure AddBrushStyleEdit(APanel: TPanel);
    procedure AddPenStyleEdit(APanel: TPanel);
    procedure AddNumOfVerticesEdit(APanel: TPanel);
    procedure RoundingXEditChange(Sender: TObject); virtual;
    procedure RoundingYEditChange(Sender: TObject); virtual;
    procedure NumOfVerticesChange(Sender: TObject);
    procedure AddLabel(AName: String; APanel: TPanel);
    procedure WidthEditChange(Sender: TObject); virtual;
    procedure PenStyleEditChange(Sender: TObject);
    procedure BrushStyleEditChange(Sender: TObject);
    procedure Init(APanel: TPanel); virtual; abstract;
    procedure MouseDown(X, Y: Integer); virtual; abstract;
    procedure MouseMove(X, Y: Integer); virtual;
    procedure MouseUp(X, Y, AWidth, AHeight: Integer; Shift: TShiftState); virtual;
  end;

  TPolylineTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TRectangleTool = class(TTool)
    constructor Create;
    procedure SetParams; override;
    procedure ToDefaultParams; override;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TEllipseTool = class(TTool)
    constructor Create;
    procedure SetParams; override;
    procedure ToDefaultParams; override;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TLineTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TRoundRectTool = class(TTool)
    constructor Create;
    procedure SetParams; override;
    procedure ToDefaultParams; override;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TPolygonTool = class(TTool)
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

procedure TRectangleTool.SetParams;
begin
  Inherited;
  (Figure as TRectangle).brushColor := brushColor;
  (Figure as TRectangle).brushStyle := brushStyle;
end;

procedure TEllipseTool.SetParams;
begin
  Inherited;
  (Figure as TEllipse).brushColor := brushColor;
  (Figure as TEllipse).brushStyle := brushStyle;
end;

procedure TRoundRectTool.SetParams;
begin
  Inherited;
  (Figure as TRoundRect).brushColor := brushColor;
  (Figure as TRoundRect).brushStyle := brushStyle;
  (Figure as TRoundRect).roundingRadiusX := roundingRadiusX;
  (Figure as TRoundRect).roundingRadiusY := roundingRadiusY;
end;

procedure TPolygonTool.SetParams;
begin
  Inherited;
  (Figure as TPolygon).brushColor := brushColor;
  (Figure as TPolygon).brushStyle := brushStyle;
  (Figure as TPolygon).numOfVertices := numOfVertices;
end;

procedure TTool.ToDefaultParams;
begin
  thickness := 1;
  penStyle := psSolid;
end;

procedure TRectangleTool.ToDefaultParams;
begin
  Inherited;
  brushStyle := bsSolid;
end;

procedure TEllipseTool.ToDefaultParams;
begin
  Inherited;
  brushStyle := bsSolid;
end;

procedure TRoundRectTool.ToDefaultParams;
begin
  Inherited;
  brushStyle := bsSolid;
  roundingRadiusX := 10;
  roundingRadiusY := 10;
end;

procedure TPolygonTool.ToDefaultParams;
begin
  Inherited;
  brushStyle := bsSolid;
  numOfVertices := 3;
end;

procedure TTool.MouseMove(X, Y: Integer);
begin
    Figure.AddPoint(X, Y, false)
end;

procedure TTool.AddLabel(AName: String; APanel: TPanel);
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

procedure TTool.WidthEditChange(Sender: TObject);
begin
  thickness := WidthEdit.Value;
end;

procedure TTool.AddWidthEdit(APanel: TPanel);
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

procedure TTool.RoundingXEditChange(Sender: TObject);
begin
  roundingRadiusX := RoundingXEdit.Value;
end;

function TTool.AddRoundingEdit(APanel: TPanel; AEditorChange: TNotifyEvent): TSpinEdit;
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
    OnChange := AEditorChange;
    Result := RoundingEdit;
  end;
end;

procedure TTool.RoundingYEditChange(Sender: TObject);
begin
  roundingRadiusY := RoundingYEdit.Value;
end;

procedure TTool.PenStyleEditChange(Sender: TObject);
begin
  penStyle := TFPPenStyle(PenStyleEdit.ItemIndex);
end;

procedure TTool.AddPenStyleEdit(APanel: TPanel);
begin
  PenStyleEdit := TComboBox.Create(APanel);
  With PenStyleEdit do begin
    Parent := APanel;
    Left := 2;
    Top := 2;
    Align := altop;
    Items.Add('─────────────────');
    Items.Add('─ ─ ─ ─ ─ ─ ─ ─ ─');
    Items.Add('• • • • • • • • •');
    Items.Add('─ • ─ • ─ • ─ • ─');
    Items.Add('─ • • ─ • • ─ • •');
    ItemIndex := 0;
    AutoComplete := False;
    Font.Bold := True;
    Font.Size := 12;
    BorderSpacing.Around:= 5;
    ReadOnly := True;
    OnChange := @PenStyleEditChange;
  end;
  AddLabel('Pen Style:', APanel);
end;

procedure TTool.BrushStyleEditChange(Sender: TObject);
begin
  brushStyle := TFPBrushStyle(BrushStyleEdit.ItemIndex);
end;

procedure TTool.AddBrushStyleEdit(APanel: TPanel);
begin
  BrushStyleEdit := TComboBox.Create(APanel);
  With BrushStyleEdit do begin
    Parent := APanel;
    Left := 2;
    Top := 2;
    Align := altop;
    Items.Add('Solid');
    Items.Add('Clear');
    Items.Add('Horizontal');
    Items.Add('Vertical');
    Items.Add('Diagonal 1');
    Items.Add('Diagonal 2');
    Items.Add('Cross');
    Items.Add('DiagCross');
    Font.Size := 10;
    ItemIndex := 0;
    BorderSpacing.Around:= 5;
    ReadOnly := True;
    OnChange := @BrushStyleEditChange;
  end;
  AddLabel('Brush Style:', APanel);
end;

procedure TTool.AddNumOfVerticesEdit(APanel: TPanel);
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

procedure TTool.NumOfVerticesChange(Sender: TObject);
begin
  NumOfVertices := NumOfVerticesEdit.Value;
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
begin
  UnselectAll;
  ParametersAvailable := true;
  AddPenStyleEdit(APanel);
  AddWidthEdit(APanel);
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
begin
  UnselectAll;
  ParametersAvailable := true;
  AddBrushStyleEdit(APanel);
  AddPenStyleEdit(APanel);
  AddWidthEdit(APanel);
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
begin
  UnselectAll;
  ParametersAvailable := true;
  AddBrushStyleEdit(APanel);
  AddPenStyleEdit(APanel);
  AddWidthEdit(APanel);
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
begin
  UnselectAll;
  ParametersAvailable := true;

  RoundingYEdit := AddRoundingEdit(APanel, @RoundingYEditChange);
  AddLabel('Rounding Y:', APanel);
  RoundingXEdit := AddRoundingEdit(APanel, @RoundingXEditChange);
  AddLabel('Rounding X:', APanel);
  AddBrushStyleEdit(APanel);
  AddPenStyleEdit(APanel);
  AddWidthEdit(APanel);
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
begin
  UnselectAll;
  ParametersAvailable := true;
  AddPenStyleEdit(APanel);
  AddWidthEdit(APanel);
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
begin
  UnselectAll;
  ParametersAvailable := true;
  AddBrushStyleEdit(APanel);
  AddPenStyleEdit(APanel);
  AddWidthEdit(APanel);
  AddNumOfVerticesEdit(APanel);
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

