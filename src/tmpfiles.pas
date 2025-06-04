unit tmpfiles;

interface

function I_NewTempFile(const name: string): string;

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
  result :=  String(buf) + '\' + ExtractFilename(name);
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


