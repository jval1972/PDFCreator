unit utils;

interface

uses
  Windows, Classes, SysUtils, Graphics;

function fix_fname(const fn: string): string;

function CopyFile(const sname, dname: string): boolean;

procedure backupfile(const fn: string);

function CountNumbers(const s: string): integer;

function IntToStr4(const x: integer): string;

function IntToStr8(const x: integer): string;

function fopen(var f: file; const FileName: string; const mode: integer): boolean;

function fsize(const FileName: string): integer;

function GetFileCreationTime(sFileName: string): TDateTime;

const
  fCreate = fmCreate or fmShareDenyWrite;
  fOpenReadOnly = fmOpenRead or fmShareDenyWrite;
  fOpenReadWrite = fmOpenReadWrite or fmShareDenyWrite;

  sFromBeginning = soFromBeginning;
  sFromCurrent = soFromCurrent;
  sFromEnd = soFromEnd;

type
  TString255 = string[255];
  PString255 = ^TString255;

function IsNumeric(const s: string): Boolean;

function sort_logical(List: TStringList; Index1, Index2: Integer): Integer;

function CBRZ2PDFConv(const fin, fout: string): boolean;

function myfindfiles(const mask: string): TStringList;

implementation

uses
  arcbase, rarfile, wzipfile, tmpfiles, jpeg, webp, options, pdfexport;

function fix_fname(const fn: string): string;
var
  p: integer;
begin
  Result := fn;
  for p := 1 to Length(Result) do
    if Result[p] = '/' then
      Result[p] := '\';
  p := Pos(':', Result);
  if p > 1 then
    if p < 10 then
      if p <> 2 then
      begin
        while p <> 2 do
        begin
          Result[1] := ' ';
          Result := Trim(Result);
          p := Pos(':', Result);
        end;
      end;
end;

function CopyFile(const sname, dname: string): boolean;
var
  FromF, ToF: file;
  NumRead, NumWritten: Integer;
  Buf: array[1..8192] of Char;
  adir: string;
begin
  Result := False;

  if (Trim(sname) = '') or (Trim(dname) = '') then
    Exit;

  if FileExists(sname) then
  begin
    {$I-}
    AssignFile(FromF, sname);
    Reset(FromF, 1);
    adir := Trim(ExtractFilePath(dname));
    if adir <> '' then
      if not DirectoryExists(adir) then
        ForceDirectories(adir);
    AssignFile(ToF, dname);
    Rewrite(ToF, 1);
    repeat
      BlockRead(FromF, Buf, SizeOf(Buf), NumRead);
      BlockWrite(ToF, Buf, NumRead, NumWritten);
    until (NumRead = 0) or (NumWritten <> NumRead);
    CloseFile(FromF);
    CloseFile(ToF);
    {$I+}
    Result := IOResult = 0;
  end;
end;

procedure backupfile(const fn: string);
var
  fbck: string;
  fname: string;
begin
  fname := Trim(fn);

  if fname = '' then
    Exit;

  if not FileExists(fname) then
    Exit;

  fbck := fname + '_' + FormatDateTime('yyyymmdd', Now);
  if FileExists(fbck) then
    fbck := fbck + '_latest';
  CopyFile(fname, fbck);
end;

function CountNumbers(const s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
      Inc(Result);
end;

function IntToStr4(const x: integer): string;
begin
  Result := IntToStr(x);
  while Length(Result) < 4 do
    Result := '0' + Result;
end;

function IntToStr8(const x: integer): string;
begin
  Result := IntToStr(x);
  while Length(Result) < 8 do
    Result := '0' + Result;
end;

function fopen(var f: file; const FileName: string; const mode: integer): boolean;
begin
  assign(f, FileName);
  {$I-}
  if mode = fCreate then
  begin
    FileMode := 2;
    rewrite(f, 1);
  end
  else if mode = fOpenReadOnly then
  begin
    FileMode := 0;
    reset(f, 1);
  end
  else if mode = fOpenReadWrite then
  begin
    FileMode := 2;
    reset(f, 1);
  end
  else
  begin
    Result := False;
    Exit;
  end;
  {$I+}
  Result := IOresult = 0;
end;

function fsize(const FileName: string): integer;
var
  f: file;
begin
  if fopen(f, FileName, fOpenReadOnly) then
  begin
  {$I-}
    Result := FileSize(f);
    close(f);
  {$I+}
  end
  else
    Result := 0;
end;

function GetFileCreationTime(sFileName: string): TDateTime;
var
  ffd: TWin32FindData;
  dft: DWORD;
  lft: TFileTime;
  h:   THandle;
begin
  //
  // get file information
  h := Windows.FindFirstFile(PChar(sFileName), ffd);
  if (INVALID_HANDLE_VALUE <> h) then
  begin
    //
    // we're looking for just one file,
    // so close our "find"
    Windows.FindClose(h);
    //
    // convert the FILETIME to
    // local FILETIME
    FileTimeToLocalFileTime(ffd.ftCreationTime, lft);
    //
    // convert FILETIME to
    // DOS time
    FileTimeToDosDateTime(lft, LongRec(dft).Hi, LongRec(dft).Lo);
    //
    // finally, convert DOS time to
    // TDateTime for use in Delphi's
    // native date/time functions
    Result := FileDateToDateTime(dft);
  end
  else
    Result := Now;
end;

function IsNumeric(const s: string): Boolean;
var
  i: integer;
begin
  Result := False;
  if s = '' then
    Exit;

  for i := 1 to Length(s) do
    if not (s[i] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']) then
      Exit;

  Result := True;
end;

function sort_logical(List: TStringList; Index1, Index2: Integer): Integer;
var
  s1, s2, buf: string;
  l1, l2: TStringList;
  i: integer;
  isnum: boolean;
  mx: integer;
  n1, n2: integer;
begin
  s1 := List.Strings[Index1];
  s2 := List.Strings[Index2];

  if (s1 = '') and (s2 = '') then
  begin
    Result := 0;
    Exit;
  end;

  if (s1 = '') or (s2 = '') then
  begin
    if List.CaseSensitive then
      Result := AnsiCompareStr(s1, s2)
    else
      Result := AnsiCompareText(s1, s2);
    Exit;
  end;

  isnum := IsNumeric(s1[1]);
  buf := '';

  l1 := TStringList.Create;
  for i := 1 to Length(s1) do
  begin
    if IsNumeric(s1[i]) <> isnum then
    begin
      l1.Add(buf);
      buf := '';
      isnum := not isnum;
    end;
    buf := buf + s1[i];
  end;
  if buf <> '' then
    l1.Add(buf);

  isnum := IsNumeric(s2[1]);
  buf := '';

  l2 := TStringList.Create;
  for i := 1 to Length(s2) do
  begin
    if IsNumeric(s2[i]) <> isnum then
    begin
      l2.Add(buf);
      buf := '';
      isnum := not isnum;
    end;
    buf := buf + s2[i];
  end;
  if buf <> '' then
    l2.Add(buf);

  if l1.Count > l2.Count then
    mx := l1.Count
  else
    mx := l2.Count;

  Result := 0;

  for i := 0 to mx - 1 do
  begin
    if IsNumeric(l1.Strings[i]) and IsNumeric(l2.Strings[i]) then
    begin
      n1 := StrToInt(l1.Strings[i]);
      n2 := StrToInt(l2.Strings[i]);
      if n1 < n2 then
        Result := -1
      else if n1 > n2 then
        Result := 1;
    end
    else
    begin
      if List.CaseSensitive then
        Result := AnsiCompareStr(l1.Strings[i], l2.Strings[i])
      else
        Result := AnsiCompareText(l1.Strings[i], l2.Strings[i]);
    end;
    if Result <> 0 then
      Break;
  end;
  if Result = 0 then
  begin
    if List.CaseSensitive then
      Result := AnsiCompareStr(s1, s2)
    else
      Result := AnsiCompareText(s1, s2);
  end;
  l1.Free;
  l2.Free;
end;

function IsZipFile(const fname: string): boolean;
var
  buf: array[0..1] of Char;
  fs: TFileStream;
begin
  Result := False;
  if not FileExists(fname) then
    Exit;

  fs := TFileStream.Create(fname, fmOpenRead);
  try
    if fs.Size > SizeOf(buf) then
    begin
      fs.Read(buf, SizeOf(buf));
      if (buf[0] = 'P') and (buf[1] = 'K') then
        Result := True;
    end;
  finally
    fs.Free;
  end;
end;

function IsRarFile(const fname: string): boolean;
var
  buf: array[0..2] of Char;
  fs: TFileStream;
begin
  Result := False;
  if not FileExists(fname) then
    Exit;

  fs := TFileStream.Create(fname, fmOpenRead);
  try
    if fs.Size > SizeOf(buf) then
    begin
      fs.Read(buf, SizeOf(buf));
      if (buf[0] = 'R') and (buf[1] = 'a') and (buf[2] = 'r') then
        Result := True;
    end;
  finally
    fs.Free;
  end;
end;

function CBRZ2PDFConv(const fin, fout: string): boolean;
var
  arc: TBaseArcFile;
  l1, l2: TStringList;
  cnt: integer;
  j: integer;
  file1, file2, ext2: string;
  JPG: TJPEGImage;
  bm: TBitmap;
begin
  Result := False;

  if IsZipFile(fin) then
    arc := TZipFile.Create(fin)
  else if IsRarFile(fin) then
    arc := TRarFile.Create(fin)
  else
    Exit;

  l1 := TStringList.Create;
  for j := 0 to arc.FileCount - 1 do
  begin
    file1 := arc.Files[j];
    ext2 := LowerCase(ExtractFileExt(file1));
    if (ext2 = '.png') or (ext2 = '.jpg') or (ext2 = '.jpeg') or (ext2 = '.webp') or (ext2 = '.bmp') or (ext2 = '.tif') or (ext2 = '.tiff') then
    begin
      l1.Add(file1);
    end;
  end;
  l1.CustomSort(sort_logical);
  l2 := TStringList.Create;
  cnt := 0;
  for j := 0 to l1.Count - 1 do
  begin
    file1 := l1.Strings[j];
    ext2 := LowerCase(ExtractFileExt(file1));
    file2 := I_NewTempFile('CBRZ2PDF' + IntToStr4(cnt) + ext2);
    if FileExists(file2) then
      DeleteFile(file2);
    l2.Add(file2);
    Inc(cnt);
  end;

  arc.ExtractFiles(l1, l2);
  arc.Free;
  l1.Free;

  if l2.Count = 0 then
  begin
    l2.Free;
    Exit;
  end;

  for j := 0 to l2.Count - 1 do
  begin
    file1 := l2.Strings[j];
    ext2 := LowerCase(ExtractFileExt(file1));
    if (ext2 = '.webp') or (ext2 = '.tif') or (ext2 = '.tiff') then
    begin
      JPG := TJPEGImage.Create;
      JPG_ReadWebp(JPG, file1);
      file1 := ChangeFileExt(file1, '.jpg');
      JPG.CompressionQuality := optjpegcompression;
      JPG.SaveToFile(file1);
      JPG.Free;
      l2.Strings[j] := file1;
    end
    else if ext2 = '.bmp' then
    begin
      JPG := TJPEGImage.Create;
      bm := TBitmap.Create;
      bm.LoadFromFile(file1);
      JPG.Assign(bm);
      bm.Free;
      file1 := ChangeFileExt(file1, '.jpg');
      JPG.CompressionQuality := optjpegcompression;
      JPG.SaveToFile(file1);
      JPG.Free;
      l2.Strings[j] := file1;
    end;
  end;

  backupfile(fout);
  MakePDFEx('', l2, fout);
  l2.Free;

  Result := True;
end;

function myfindfiles(const mask: string): TStringList;
var
  sr: TSearchRec;
  path: string;
begin
  Result := TStringList.Create;
  path := ExtractFilePath(mask);
  if FindFirst(mask, faAnyFile, sr) = 0 then
  begin
    Result.Add(path + sr.Name);
    while FindNext(sr) = 0 do
      Result.Add(path + sr.Name);
    FindClose(sr);
  end;
end;

end.
