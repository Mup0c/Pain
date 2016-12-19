unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, math, Grids, Spin, UAbout,
  UTools, UFigures, UTransform, LCLType, Types;

type

  { TMainScreen }

  TMainScreen = class(TForm)
    ColorDialog: TColorDialog;
    DrawGrid: TDrawGrid;
    ScaleEdit: TFloatSpinEdit;
    MenuFullExtent: TMenuItem;
    MenuUndo: TMenuItem;
    PaletteLabel: TLabel;
    RowsLabel: TLabel;
    ColsLabel: TLabel;
    ScaleLabel: TLabel;
    HorizontalScrollBar: TScrollBar;
    VerticalScrollBar: TScrollBar;
    SelectedBrushColor: TPanel;
    SelectedPenColor: TPanel;
    Polyline_btn: TBitBtn;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuExit: TMenuItem;
    MenuAbout: TMenuItem;
    MenuClear: TMenuItem;
    PaintField: TPaintBox;
    Rect_btn: TBitBtn;
    Ellipse_btn: TBitBtn;
    Line_btn: TBitBtn;
    ColsEdit: TSpinEdit;
    RowsEdit: TSpinEdit;
    ToolPanel: TPanel;
    procedure ColsEditChange(Sender: TObject);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure HorizontalScrollBarChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuClearClick(Sender: TObject);
    procedure MenuFullExtentClick(Sender: TObject);
    procedure MenuUndoClick(Sender: TObject);
    procedure PaintFieldMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintFieldMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintFieldMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintFieldPaint(Sender: TObject);
    procedure DrawGridRepaint;
    procedure RowsEditChange(Sender: TObject);
    procedure PaintFieldMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PaintFieldMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ScaleEditChange(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure SetScroolBarsParameters(ARect: TDoubleRect);
    procedure VerticalScrollBarChange(Sender: TObject);
    procedure PanelInit;
    procedure ButtonInit;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainScreen: TMainScreen;
  ScrollsChangingByCode, EditValueChangedByCode, isMouseDown: boolean;
  colors: array of array of TColor;
  paletteRows: integer = 2;
  paletteCols: integer = 3;
  ToolParameters: TPanel;

implementation

{$R *.lfm}

{ TMainScreen }

procedure SaveFigure(Figure: TFigure);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := Figure;
  if length(Figures) = 1 then begin
    imageBounds := Figure.GetBounds;
  end;
end;

procedure ClearScreen;
begin
  Figures := nil;
  MainScreen.Canvas.Clear;
  MainScreen.Repaint;
end;

procedure TMainScreen.MenuClearClick(Sender: TObject);
begin
  ClearScreen;
end;

procedure TMainScreen.MenuFullExtentClick(Sender: TObject);
var imageWidth,imageHeight,AScale: double;
begin
  imageWidth := imageBounds.Right - imageBounds.Left;
  imageHeight := imageBounds.Bottom -  imageBounds.Top;
  if (imageWidth <> 0) and (imageHeight <> 0) then begin
    AScale := min(((PaintField.Height - 10) / (imageHeight)), ((PaintField.Width - 10) / (imageWidth)));
    EditValueChangedByCode := true;
    ScaleEdit.Value := AScale * 100;
    EditValueChangedByCode := false;
    Scale := AScale;
    SetCanvasPosition(imageBounds.Left - 5/scale,imageBounds.Top - 5/scale);
    PaintField.Invalidate;
  end;
end;

procedure TMainScreen.MenuUndoClick(Sender: TObject);
begin
  if Length(Figures) > 0 then
    SetLength(Figures,length(Figures) - 1);
  MainScreen.Repaint;
end;

procedure TMainScreen.MenuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainScreen.PanelInit;
begin
  ToolParameters := TPanel.Create(nil);
  With ToolParameters do begin
    Name := 'ToolParameters';
    Caption := '';
    Parent := MainScreen;
    Align := alRight;
    Width := 100;
    Color := clDefault;
    Visible := false;
  end;
end;

procedure TMainScreen.ButtonInit;
var i: integer; ToolIcon: TBitmap;
begin
  CurrentTool := ToolRegistry[0];
  for i := Low(ToolRegistry) to High(ToolRegistry) do begin
    ToolIcon := TBitmap.Create;
    ToolIcon.LoadFromFile(ToolRegistry[i].bmpName);
    With TBitBtn.Create(Self) do begin
      Parent := ToolPanel;
      Tag := i;
      Width := 32;
      Height := 32;
      if (i+1) mod 2 = 1 then begin
        Left := 8;
        Top := (i div 2)*40 + 8;
      end else
      begin
        Left := 56;
        Top := ((i-1) div 2)*40 + 8;
      end;
      OnClick := @ToolClick;
      Glyph := ToolIcon;
    end;
  end;
end;

procedure TMainScreen.FormCreate(Sender: TObject);
begin
  DrawGridRepaint;
  ButtonInit;
  PanelInit;
end;

procedure TMainScreen.ToolClick(Sender: TObject);
begin
  ToolParameters.Destroy;
  CurrentTool := ToolRegistry[(Sender as TBitBtn).Tag];
  if (Sender as TBitBtn).Tag = 0 then PaintField.Cursor := crSizeAll else
    PaintField.Cursor := crDefault;
  PanelInit;
  CurrentTool.Init(ToolParameters);
  CurrentTool.brushColor := SelectedBrushColor.Color;
  CurrentTool.penColor := SelectedPenColor.Color;
  if CurrentTool.ParametersAvailable then
    begin
      VerticalScrollBar.Left := MainScreen.Width - 115;
      ToolParameters.visible := true;
    end
    else
      VerticalScrollBar.Left := MainScreen.Width - 15;
  PaintField.Invalidate;
end;

procedure TMainScreen.DrawGridRepaint;
var i,j,rowGradient,colGradient,diaGradient: integer;
begin
  DrawGrid.ColCount := paletteCols;
  DrawGrid.RowCount := paletteRows;
  SetLength(colors,paletteCols);
  DrawGrid.Height := paletteRows * 20 + 1;
  for i := 0 to paletteCols - 1 do
    SetLength(colors[i],paletteRows);
  for j := 0 to paletteRows - 1 do begin
    rowGradient := floor(j / (paletteRows - 1) * 255);
    colors[0][j] := RGBToColor(rowGradient, rowGradient, rowGradient);
  end;
  for i := 0 to paletteCols - 2 do
    for j := 0 to paletteRows - 1 do begin
      rowGradient := floor(j / (paletteRows - 1) * 255);
      colGradient := floor(i / (paletteCols - 2) * 255);
      diaGradient := floor((255 - colGradient) * (255 - rowGradient) / 255);
      colors[i + 1][j] := RGBToColor(colGradient, rowGradient, diaGradient);
    end;
end;

procedure TMainScreen.DrawGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  DrawGrid.Canvas.Brush.Color := colors[aCol][aRow];
end;

procedure TMainScreen.DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var aCol, aRow: integer;
begin
  DrawGrid.MouseToCell(x,y,aCol,aRow);
  if Button = mbLeft then begin
    SelectedPenColor.Color := colors[aCol][aRow];
    CurrentTool.penColor := colors[aCol][aRow];
  end else
  if Button = mbRight then begin
    SelectedBrushColor.Color := colors[aCol][aRow];
    CurrentTool.brushColor := colors[aCol][aRow];
  end;
end;

procedure TMainScreen.DrawGridDblClick(Sender: TObject);
begin
  if ColorDialog.execute then begin
    colors[DrawGrid.Col][DrawGrid.Row] := ColorDialog.Color;
    SelectedPenColor.Color := colors[DrawGrid.Col][DrawGrid.Row];
    CurrentTool.penColor := colors[DrawGrid.Col][DrawGrid.Row];
  end;
end;

procedure TMainScreen.RowsEditChange(Sender: TObject);
var temppaletteRows: Integer;
begin
  temppaletteRows := paletteRows;
  paletteRows := RowsEdit.Value;
  DrawGridRepaint;
  MainScreen.Height :=  MainScreen.Height + (paletteRows - temppaletteRows) * 20;
end;

procedure TMainScreen.PaintFieldMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if not isMouseDown then
    SetScale(Scale - Scale / 10, MousePos.x, MousePos.y);
  PaintField.Invalidate;
end;

procedure TMainScreen.PaintFieldMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if not isMouseDown then
    SetScale(Scale + Scale / 10, MousePos.x, MousePos.y);
  PaintField.Invalidate;
end;

procedure TMainScreen.ScaleEditChange(Sender: TObject);
begin
  if not EditValueChangedByCode then begin
    SetScale(ScaleEdit.Value / 100, PaintField.Width  div 2, PaintField.Height  div 2);
    PaintField.Invalidate;
  end;
end;

procedure TMainScreen.ColsEditChange(Sender: TObject);
begin
  paletteCols := ColsEdit.Value;
  DrawGridRepaint;
  if (MainScreen.Width < 20 * paletteCols + 1) then
    MainScreen.Width := 20 * paletteCols + 1;
end;

procedure TMainScreen.MenuAboutClick(Sender: TObject);
begin
   About.show;
end;

procedure TMainScreen.PaintFieldMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    isMouseDown := true;
    CurrentTool.MouseDown(X, Y);
    PaintField.Invalidate;
  end;
end;

procedure TMainScreen.PaintFieldMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if isMouseDown then begin
    CurrentTool.MouseMove(X, Y);
    PaintField.Invalidate;
  end;
end;

procedure TMainScreen.PaintFieldMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  b: TDoubleRect;
begin
  if Button = mbLeft then begin
    isMouseDown := false;
    CurrentTool.MouseUp(X, Y, PaintField.Width, PaintField.Height, Shift);
    if CurrentTool.Figure <> nil then begin
      SaveFigure(CurrentTool.GetFigure);
    end;
    for i := 0 to High(Figures) do begin
      b := Figures[i].GetBounds;
      if i = 0 then
        imageBounds := b;
      AdjustImageBounds(b.Left, b.Top);
      AdjustImageBounds(b.Right, b.Bottom);
    end;
    PaintField.Invalidate;
  end;
end;

procedure TMainScreen.PaintFieldPaint(Sender: TObject);
var i:integer;
begin
  for i := 0 to High(Figures) do
    begin
      Figures[i].Draw(PaintField.Canvas);
      if Figures[i].Selected then begin
        With PaintField.Canvas do begin
          Brush.Color := clRed;
          Pen.Color := clRed;
          Pen.Width := PaintField.Canvas.Pen.Width + 2;
          Brush.Style := bsDiagCross;
          Pen.Style := psDash;
        end;
        Figures[i].DrawFigure(PaintField.Canvas, false);
        With PaintField.Canvas do begin
          Brush.Color := clBlue;
          Pen.Color := clBlue;
          Pen.Style := psDot;
          Brush.Style := bsCross;
        end;
        Figures[i].DrawFigure(PaintField.Canvas, false);
      end;
    end;
  EditValueChangedByCode := true;
  ScaleEdit.Value := Scale * 100;
  EditValueChangedByCode := false;
  if isMouseDown then
    if CurrentTool.Figure <> nil then
      CurrentTool.GetFigure.Draw(PaintField.Canvas);
  AdjustCanvasBounds(DoubleRect(ScrToWorld(0,0),ScrToWorld(PaintField.Width,PaintField.Height)));
  ScrollsChangingByCode:= true;
    SetScroolBarsParameters(canvasBounds);
  ScrollsChangingByCode:= false;
end;

procedure TMainScreen.SetScroolBarsParameters(ARect: TDoubleRect);
var
  HPos,HMin,HMax,HPage,VPos,VMin,VMax,VPage: int64;
begin
  HPos := round(ScrToWorld(0,0).X);
  HPage := round(PaintField.Width/scale);
  HMin := round(ARect.Left);
  HMax := round(ARect.Right);
  HorizontalScrollBar.SetParams(HPos,HMin,HMax,HPage);
  VPos := round(ScrToWorld(0,0).Y);
  VPage := round(PaintField.Height/scale);
  VMin := round(ARect.Top);
  VMax := round(ARect.Bottom);
  VerticalScrollBar.SetParams(VPos,VMin,VMax,VPage);
end;

procedure TMainScreen.VerticalScrollBarChange(Sender: TObject);
begin
  with VerticalScrollBar do begin
    If Position > Max - PageSize then
      Position := Max - PageSize;
  end;
  if not ScrollsChangingByCode then begin
  SetCanvasPosition(HorizontalScrollBar.Position, VerticalScrollBar.position);
  PaintField.Invalidate;
  end;
end;

procedure TMainScreen.HorizontalScrollBarChange(Sender: TObject);
begin
  with HorizontalScrollBar do begin
    If Position > Max - PageSize then
      Position := Max - PageSize;
  end;
  if not ScrollsChangingByCode then begin
  SetCanvasPosition(HorizontalScrollBar.Position, VerticalScrollBar.position);
  PaintField.Invalidate;
  end;
end;

end.

