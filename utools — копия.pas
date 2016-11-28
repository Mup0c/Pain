unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, math, Grids, Spin,
  UFigures, UMove, LCLType, Types, FPCanvas;

type

  TTool = class
    bmpName: String;
    Figure: TFigure;
    thickness: Integer;
    penColor: TColor;
    brushColor: TColor;
    penStyle: TFPPenStyle;
    brushStyle: TFPBrushStyle;
    numOfVertices: integer;
    ParametersAvailable: boolean;
    WidthEdit: TSpinEdit;
    PenStyleEdit: TComboBox;
    BrushStyleEdit: TComboBox;
    NumOfVerticesEdit: TSpinEdit;
    function GetFigure: TFigure; virtual;
    procedure SetParams;
    procedure ToDefaultParams;
    procedure AddWidthEdit(APanel: TPanel); virtual;
    procedure AddBrushStyleEdit(APanel: TPanel);
    procedure AddPenStyleEdit(APanel: TPanel);
    procedure AddNumOfVerticesEdit(APanel: TPanel);
    procedure NumOfVerticesChange(Sender: TObject);
    procedure AddLabel(AName: String; APanel: TPanel);
    procedure WidthEditChange(Sender: TObject); virtual;
    procedure PenStyleEditChange(Sender: TObject);
    procedure BrushStyleEditChange(Sender: TObject);
    procedure Init(APanel: TPanel); virtual; abstract;
    procedure MouseDown(X, Y: Integer); virtual; abstract;
    procedure MouseMove(X, Y: Integer); virtual;
    procedure MouseUp(X, Y, AWidth, AHeight: Integer); virtual;
  end;

  TPolylineTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TRectangleTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TEllipseTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TLineTool = class(TTool)
    constructor Create;
    procedure Init(APanel: TPanel); override;
    procedure MouseDown(X, Y: Integer); override;
  end;

  TPolygonTool = class(TTool)
    constructor Create;
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
    procedure MouseUp(X, Y, AWidth, AHeight: Integer); override;
  end;

var
  ToolRegistry: array of TTool;

implementation

procedure TTool.MouseUp(X, Y, AWidth, AHeight: Integer);
begin
end;

function TTool.GetFigure: TFigure;
begin
  Result := Figure;
end;

procedure TTool.SetParams;
begin
  Figure.thickness := thickness;
  Figure.penColor := penColor;
  Figure.brushColor := brushColor;
  Figure.penStyle := penStyle;
  Figure.brushStyle := brushStyle;
  Figure.numOfVertices := numOfVertices;
end;

procedure TTool.ToDefaultParams;
begin
  thickness := 1;
  penStyle := psSolid;
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
    Top := Y;
    Left := X;
    Bottom := Y;
    Right := X;
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
  ParametersAvailable := true;
  AddBrushStyleEdit(APanel);
  AddPenStyleEdit(APanel);
  AddWidthEdit(APanel);
  ToDefaultParams;
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
  ParametersAvailable := true;
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
  ParametersAvailable := true;
  AddBrushStyleEdit(APanel);
  AddPenStyleEdit(APanel);
  AddWidthEdit(APanel);
  AddNumOfVerticesEdit(APanel);
  ToDefaultParams;
end;

procedure TDragTool.MouseDown(X, Y: Integer);
begin
  prevCrds := ScrToWorldCrds(X,Y);
end;

procedure TDragTool.MouseMove(X, Y: Integer);
begin
  CanvasMove(prevCrds.X - ScrToWorldCrds(X,Y).X, prevCrds.Y - ScrToWorldCrds(X,Y).Y);
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
  FirstPoint := ScrToWorldCrds(X, Y);
end;

procedure TZoomToTool.MouseMove(X, Y: Integer);
begin
  Figure.AddPoint(X, Y, false);
end;

procedure TZoomToTool.MouseUp(X, Y, AWidth, AHeight: Integer);
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
RegisterTool(TPolyLineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);
RegisterTool(TPolygonTool.Create);
RegisterTool(TZoomInTool.Create);
RegisterTool(TZoomOutTool.Create);
RegisterTool(TZoomToTool.Create);


end.

