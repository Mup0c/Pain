unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, FPCanvas, Graphics;

const
   BufferSize = 30;

type

  TCircularBuffer = class
    CurrentPosition, AvailableUndos, AvailableRedos, SavedPosition: integer;
    HistoryBuffer: array [1..BufferSize] of array of StrArr;
    procedure AddToBuffer;
    procedure Undo;
    procedure Redo;
  end;

var
  History: TCircularBuffer;
  FileWasChanged: boolean;

implementation

procedure TCircularBuffer.AddToBuffer;
var
  i: Integer;
begin
  if AvailableUndos < BufferSize - 1 then
    Inc(AvailableUndos);
  if CurrentPosition = BufferSize then
    CurrentPosition := 1
  else
    Inc(CurrentPosition);
  if CurrentPosition = SavedPosition then SavedPosition := -1;
  SetLength(HistoryBuffer[CurrentPosition],Length(Figures));
  for i:= 0 to High(Figures) do begin
    HistoryBuffer[CurrentPosition][i] := Figures[i].Save;
  end;
  AvailableRedos := 0;
end;

procedure LoadFromStr(ACurrentBufferState: array of StrArr);
var
  NumOfParameters, j, i: integer;
  Parameters: StrArr;
  FigureName: String;
begin
  SetLength(Figures, Length(ACurrentBufferState));
  for i:= 0 to High(ACurrentBufferState) do begin
    FigureName := ACurrentBufferState[i][0];
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
      Parameters[j] := ACurrentBufferState[i][j + 1];
    Figures[i].Load(Parameters);
  end;
end;

procedure TCircularBuffer.Undo;
var
  NumOfParameters, j, i: integer;
  Parameters: StrArr;
  FigureName: String;
begin
  if AvailableUndos > 0 then begin
    Dec(AvailableUndos);
    Inc(AvailableRedos);
    if CurrentPosition = 1 then
      CurrentPosition := BufferSize
    else
      Dec(CurrentPosition);
    LoadFromStr(HistoryBuffer[CurrentPosition]);
    if CurrentPosition = SavedPosition then
      FileWasChanged := False
    else
      FileWasChanged := True;
  end;
end;

procedure TCircularBuffer.Redo;
var
  NumOfParameters, j, i: integer;
  Parameters: StrArr;
  FigureName: String;
begin
  if AvailableRedos > 0 then begin
    Inc(AvailableUndos);
    Dec(AvailableRedos);
    if CurrentPosition = BufferSize then
      CurrentPosition := 1
    else
      Inc(CurrentPosition);
    LoadFromStr(HistoryBuffer[CurrentPosition]);
    if CurrentPosition = SavedPosition then
      FileWasChanged := False
    else
      FileWasChanged := True;
  end;
end;

initialization
History := TCircularBuffer.Create;

end.
