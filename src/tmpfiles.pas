unit tmpfiles;

interface

function I_NewTempFile(const name: string): string;

function I_NewGlobalTempFile(const name: string): string;

function I_DeclareTempFile(const fname: string): string;

procedure I_ClearAllTempFiles;

implementation

uses
  Windows,
  SysUtils,
  Classes,
  utils;

var
  tempfiles: TStringList;
  cntid: integer = 0;

function I_NewTempFile(const name: string): string;
var
  buf: array[0..4095] of char;
begin
  ZeroMemory(@buf, SizeOf(buf));
  GetTempPath(SizeOf(buf), buf);
  result := String(buf) + '\' + ExtractFilename(name);
  tempfiles.Add(result);
  inc(cntid);
end;

function I_NewGlobalTempFile(const name: string): string;
var
  buf: array[0..4095] of char;
  aname: string;
  num: integer;
begin
  ZeroMemory(@buf, SizeOf(buf));
  GetTempPath(SizeOf(buf), buf);
  aname := ExtractFilename(name);
  if Length(aname) > 9 then
    if IsNumeric(aname[1]) and IsNumeric(aname[2]) and IsNumeric(aname[3]) and
       IsNumeric(aname[4]) and IsNumeric(aname[5]) and IsNumeric(aname[6]) and
       IsNumeric(aname[7]) and IsNumeric(aname[8]) and Pos('_', aname) > 0 then
    begin
      num := StrToInt(Copy(s, 1, 8));
      if cntid <= num then
        cntid := num + 1;
      aname[1] := ' ';
      aname[2] := ' ';
      aname[3] := ' ';
      aname[4] := ' ';
      aname[5] := ' ';
      aname[6] := ' ';
      aname[7] := ' ';
      aname[8] := ' ';
      if aname[9] = '_' then
        aname[9] := ' ';
      aname := Trim(aname);
    end;
  result := String(buf) + '\' + IntToStr8(cntid) + '_' + aname;
  tempfiles.Add(result);
  inc(cntid);
end;

function I_DeclareTempFile(const fname: string): string;
begin
  tempfiles.Add(fname);
  Result := fname;
end;

procedure I_ClearAllTempFiles;
var
  iii: integer;
begin
  for iii := 0 to tempfiles.Count - 1 do
    if FileExists(tempfiles.Strings[iii]) then
      DeleteFile(tempfiles.Strings[iii]);
  tempfiles.Clear;
end;

initialization
  tempfiles := TStringList.Create;

finalization
  I_ClearAllTempFiles;
  tempfiles.Free;

end.


