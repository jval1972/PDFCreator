program CBRZ2PDF;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  img_names in 'img_names.pas',
  img_utils in 'img_utils.pas',
  libwebp in 'libwebp.pas',
  pngimage in 'pngimage.pas',
  pnglang in 'pnglang.pas',
  utils in 'utils.pas',
  webp in 'webp.pas',
  mORMotReport in 'SynPDF\mORMotReport.pas',
  SynCommons in 'SynPDF\SynCommons.pas',
  SynCrypto in 'SynPDF\SynCrypto.pas',
  SynGdiPlus in 'SynPDF\SynGdiPlus.pas',
  SynLZ in 'SynPDF\SynLZ.pas',
  SynPdf in 'SynPDF\SynPdf.pas',
  SynTable in 'SynPDF\SynTable.pas',
  SynZip in 'SynPDF\SynZip.pas',
  wzipfile in 'wzipfile.pas',
  zlibpas in 'zlibpas.pas',
  pdf2jpg in 'pdf2jpg.pas',
  options in 'options.pas',
  pdfexport in 'pdfexport.pas',
  webpimage in 'webpimage.pas',
  tmpfiles in 'tmpfiles.pas',
  BTMemoryModule in 'BTMemoryModule.pas',
  unrar in 'unrar.pas',
  zlibpasEx in 'zlibpasEx.pas',
  rarfile in 'rarfile.pas',
  unrardll in 'unrardll.pas',
  arcbase in 'arcbase.pas',
  LibDelphi in 'LibDelphi.pas',
  LibJpegDelphi in 'LibJpegDelphi.pas',
  LibTiffDelphi in 'LibTiffDelphi.pas',
  ZLibDelphi in 'ZLibDelphi.pas',
  xTIFF in 'xTIFF.pas';

var
  i, j: integer;
  l, l2, l3: TStringList;
  s, s2, ext, path, tpath: string;
begin
  { TODO -oUser -cConsole Main : Insert code here }
  l := TStringList.Create;

  for i := 1 to ParamCount do
  begin
    s := ParamStr(i);
    ext := LowerCase(ExtractFileExt(s));
    if (ext = '.cbr') or (ext = '.cbz') then
      l.Add(s)
    else if ext = '.txt' then
    begin
      l2 := TStringList.Create;
      try
        l2.LoadFromFile(s);
        tpath := ExtractFilePath(s);
        for j := 0 to l2.Count - 1 do
        begin
          s := l2.Strings[j];
          ext := LowerCase(ExtractFileExt(s));
          if (ext = '.cbr') or (ext = '.cbz') or (ext = '.zip') or (ext = '.rar') then
          begin
            path := ExtractFilePath(s);
            if path = '' then
              l.Add(tpath + s)
            else
              l.Add(s);
          end;
        end;
      finally
        l2.Free;
      end;
    end
    else if DirectoryExists(s) and (s <> '') then
    begin
      if s[Length(s)] <> '\' then
        s := s + '\';
      l2 := myfindfiles(s + '*.*');
      for j := 0 to l2.Count - 1 do
      begin
        s := l2.Strings[j];
        ext := LowerCase(ExtractFileExt(s));
        if (ext = '.cbr') or (ext = '.cbz') or (ext = '.zip') or (ext = '.rar') then
          l.Add(s)
      end;
      l2.Free;
    end
    else if (Pos('*', s) > 0) or (Pos('?', s) > 0) then
    begin
      l2 := myfindfiles(s);
      for j := 0 to l2.Count - 1 do
      begin
        s := l2.Strings[j];
        ext := LowerCase(ExtractFileExt(s));
        if (ext = '.cbr') or (ext = '.cbz') or (ext = '.zip') or (ext = '.rar') then
          l.Add(s)
      end;
      l2.Free;
    end;
  end;
  if l.Count = 0 then
  begin
    writeln('CBRZ2PDF v.1.0.');
    writeln('Please specify in command line the CBR/CBZ you want to convert to PDF');
    readln;
    l.Free;
    Halt(0);
  end;

  for i := 0 to l.Count - 1 do
  begin
    s := l.Strings[i];
    s2 := ChangeFileExt(s, '.pdf');
    write('Converting ' + ExtractFileName(s) + '...');
    if CBRZ2PDFConv(s, s2) then
      writeln('...Done!')
    else
      writeln('...Failed!')
  end;
  l.Free;
end.
