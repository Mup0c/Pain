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
    MenuRedo: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSave: TMenuItem;
    MenuSaveAs: TMenuItem;
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
    SaveImageDialog: TSaveDialog;
    OpenImageDialog: TOpenDialog;
    procedure ColsEditChange(Sender: TObject);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure HorizontalScrollBarChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuClearClick(Sender: TObject);
    procedure MenuFullExtentClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuRedoClick(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure WriteToFile(AFileName: string);
    procedure UpdateFileName;
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
    procedure DestroyPanelEditors;
    procedure CreatePanel;
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
  Signature: string = '@PaintEmulatorFormat';
  ToolParameters: TPanel;
  ImageName, LastSavedFileName: string;
  CanExit: boolean;

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

procedure TMainScreen.MenuOpenClick(Sender: TObject);
var
 f: TextFile;
 i, j, NumOfFigures, NumOfParameters: Integer;
 FileSignature, FigureName: String;
 Parameters: StrArr;
 b: TDoubleRect;
begin
  OpenImageDialog := TOpenDialog.Create(Self);
  with OpenImageDialog do begin
    InitialDir := GetCurrentDir;
    Filter     := 'Paint Emulator Format|*.pef|';
    Title      := 'Open File';
    DefaultExt := 'pef';
  end;
  if OpenImageDialog.Execute then begin
    AssignFile(f, OpenImageDialog.FileName);
    Reset(f);
    Readln(f, FileSignature);
    if (FileSignature <> Signature) then begin
      ShowMessage('Invalid file');
      CloseFile(f);
      Exit;
    end;
    Readln(f, NumOfFigures);
    CurrentTool.Figure := nil;
    SetLength(Figures, NumOfFigures);
    for i := 0 to high(Figures) do begin
      Readln(f, FigureName);
      case FigureName of
        'TPolyline':
          begin
            Figures[i] := TPolyline.Create;
            NumOfParameters := 4;
          end;
        'TLine':
          begin
            Figures[i] := TLine.Create;
            NumOfParameters := 4;
          end;
        'TRectangle':
          begin
            Figures[i] := TRectangle.Create;
            NumOfParameters := 6;
          end;
        'TEllipse':
          begin
            Figures[i] := TEllipse.Create;
            NumOfParameters := 6;
          end;
        'TRoundRect':
          begin
            Figures[i] := TRoundRect.Create;
            NumOfParameters := 8;
          end;
        'TPolygon':
          begin
            Figures[i] := TPolygon.Create;
            NumOfParameters := 7;
          end;
      end;
      SetLength(Parameters, NumOfParameters);
      for j := 0 to High(Parameters) do
        Readln(f, Parameters[j]);
      Figures[i].Load(Parameters);
    end;
    LastSavedFileName := OpenImageDialog.FileName;
    ImageName := OpenImageDialog.FileName;
    FileWasChanged:= false;
    UpdateFileName;
    CloseFile(f);
    SetScale(1,0,0);
    SetCanvasPosition(0,0);
    MainScreen.Invalidate;
  end;
end;

procedure TMainScreen.MenuSaveAsClick(Sender: TObject);
var
  f: TextFile;
  Reply, BoxStyle: Integer;
begin
  CanExit := false;
  SaveImageDialog := TSaveDialog.Create(self);
  with SaveImageDialog do begin
    InitialDir := GetCurrentDir;
    Title      := 'Save image as Paint Emulator Format';
    DefaultExt := 'pef';
    Filter     := 'Paint Emulator Format|*.pef|';
    FileName   := ImageName;
  end;
  if SaveImageDialog.Execute then begin
    if FileExists(SaveImageDialog.FileName) then begin
      BoxStyle := MB_ICONQUESTION + MB_YESNO;
      Reply := Application.MessageBox('Overwrite file?', 'File already exists', BoxStyle);
      if (Reply = IDYES) then begin
        WriteToFile(SaveImageDialog.FileName);
        CanExit:= true;
      end
      else begin
        SaveImageDialog.Free;
        MenuSaveAsClick(TObject.Create);
        Exit;
      end;
    end
    else begin
      WriteToFile(SaveImageDialog.FileName);
      CanExit:= true;
    end;
  end;
  SaveImageDialog.Free;
end;

procedure TMainScreen.MenuSaveClick(Sender: TObject);
begin
  if (LastSavedFileName = ImageName) then
    WriteToFile(ImageName)
  else
    MenuSaveAsClick(TObject.Create);
end;

procedure TMainScreen.WriteToFile(AFileName: string);
var
  f: TextFile;
  i,j: integer;
  figureStr: StrArr;
begin
  AssignFile(f,AFileName);
  DeleteFile(AFileName);
  Rewrite(f);
  Writeln(f, Signature);
  Writeln(f, length(Figures));
  for i := Low(Figures) to High(Figures) do
  begin
    figureStr := Figures[i].Save;
    for j := 0 to High(figureStr) do
      Writeln(f, figureStr[j]);
  end;
  CloseFile(f);
  ImageName := AFileName;
  LastSavedFileName := AFileName;
  FileWasChanged := False;
  UpdateFileName;
end;

 procedure TMainScreen.UpdateFileName;
 var
   tmpCaption: String;
 begin
   tmpCaption := 'paint_dark_moon_editi0n - ' + ImageName;
   if FileWasChanged then tmpCaption := tmpCaption + '*';
   MainScreen.Caption := tmpCaption;
 end;

procedure TMainScreen.MenuRedoClick(Sender: TObject);
begin

end;

procedure TMainScreen.MenuUndoClick(Sender: TObject);
begin

end;

procedure TMainScreen.MenuExitClick(Sender: TObject);
var
  IntMessageDialog: integer;
begin
  if FileWasChanged then begin
    IntMessageDialog := MessageDLG('Save current Image?', mtConfirmation, [mbYes,mbNo,mbCancel],0);
    if IntMessageDialog = mrYes then begin
      MenuSaveAsClick(TObject.Create);
      if CanExit then
        Application.Terminate;
    end else
      if IntMessageDialog = mrNo then
        Application.Terminate;
  end else
    Application.Terminate;
end;

procedure TMainScreen.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  IntMessageDialog: integer;
begin
  CanClose := False;
  if FileWasChanged then begin
    IntMessageDialog := MessageDLG('Save current Image?', mtConfirmation, [mbYes,mbNo,mbCancel],0);
    if IntMessageDialog = mrYes then begin
      MenuSaveAsClick(TObject.Create);
      if CanExit then
        CanClose:= True;
    end else
      if IntMessageDialog = mrNo then
        CanClose:= True;
  end else
    CanClose:= True;
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

procedure TMainScreen.DestroyPanelEditors;
begin
  ToolParameters.Free;
end;

procedure TMainScreen.CreatePanel;
begin
  PanelInit;
  if CurrentTool.ParametersAvailable then
  begin
    VerticalScrollBar.Left := MainScreen.Width - 115;
    ToolParameters.visible := true;
  end
  else
    VerticalScrollBar.Left := MainScreen.Width - 15;
end;

procedure TMainScreen.FormCreate(Sender: TObject);
var i :integer;
begin
  DrawGridRepaint;
  ButtonInit;
  PanelInit;
  for i := 0 to high(ToolRegistry) do begin
    ToolRegistry[i].Init(ToolParameters);
  end;
  InvalidateHandler:=@PaintField.Invalidate;
  DestroyPanelHandler := @DestroyPanelEditors;
  CreatePanelHandler:= @CreatePanel;
  ImageName := 'Unnamed.pef';
end;

procedure TMainScreen.ToolClick(Sender: TObject);
begin
  ToolParameters.Free;
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
var aCol, aRow, i: integer;
begin
  DrawGrid.MouseToCell(x,y,aCol,aRow);
  if Button = mbLeft then begin
    SelectedPenColor.Color := colors[aCol][aRow];
    CurrentTool.penColor := colors[aCol][aRow];
    for i := 0 to High(Figures) do begin
      If Figures[i].Selected then Figures[i].penColor:= colors[aCol][aRow];
    end;
  end else
  if Button = mbRight then begin
    SelectedBrushColor.Color := colors[aCol][aRow];
    CurrentTool.brushColor := colors[aCol][aRow];
    for i := 0 to High(Figures) do begin
      If Figures[i].Selected then Figures[i].brushColor:= colors[aCol][aRow];
    end;
  end;
  if CurrentTool.ClassName = TSelectorTool.ClassName then
    FileWasChanged := True;
  PaintField.Invalidate;
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
    CurrentTool.MouseUp(X, Y, PaintField.Width, PaintField.Height, Shift, ToolParameters);
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
  UpdateFileName;
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

