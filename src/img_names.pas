unit img_names;

interface

function GetImgName(const idx: Integer): string;

implementation

uses
  SysUtils, Classes, utils;

var
  pt1: TStringList;
  pt2: TStringList;
  pt3: TStringList;
  pt4: TStringList;

function rnd_list(const l: TStringList): string;
begin
  if l.Count = 0 then
    Result := ''
  else if l.Count = 1 then
    Result := l[0]
  else
    Result := l[Random(l.Count)];
end;

function GetImgName(const idx: Integer): string;
begin
  Result := IntToStr(idx);
  while Length(Result) < 4 do
    Result := '0' + Result;

  if Random(100) < 10 then
  begin
    Result := Result + rnd_list(pt4);
    Exit;
  end;

  Result := Result + rnd_list(pt1);
  Result := Result + rnd_list(pt2);
  Result := Result + rnd_list(pt3);
end;

initialization
  Randomize;

  pt1 := TStringList.Create;
  pt2 := TStringList.Create;
  pt3 := TStringList.Create;
  pt4 := TStringList.Create;

finalization
  pt1.Free;
  pt2.Free;
  pt3.Free;
  pt4.Free;

end.
