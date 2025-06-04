unit pdf2jpg;

interface

uses
  SysUtils, Classes, jpeg;

type
  pdf2jpegprogressevent = procedure(const pct: Single) of Object;

  TPDF2Jpeg = class(TObject)
  private
    { Private declarations }
    Limages: TStringList;
    globalid: integer;
    procedure ClearImages;
  public
    { Public declarations }
    OnProgress: pdf2jpegprogressevent;
    reverseorder: Boolean;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function OpenPDFFromFile(const fname: string): integer;
    function AppendPDFFromFile(const fname: string): integer;
    function OpenPDFFromData(const adata: string): integer;
    function AppendPDFFromData(const adata: string): integer;
    procedure SaveImagesToZip(const fname: string);
    procedure SaveImagesToDir(const apath: string);
    procedure SaveImageTo(const idx: integer; const apath: string);
    function Count: integer;
    function GetNewJPEGFromIndex(const idx: integer): TJPEGImage;
  end;

procedure PDF2ZIP(const pname, zname: string);

implementation

uses
  wzipfile;

constructor TPDF2Jpeg.Create;
begin
  Limages := TStringList.Create;
  OnProgress := nil;
  globalid := 0;
  reverseorder := False;
  Inherited;
end;


destructor TPDF2Jpeg.Destroy;
begin
  ClearImages;
  Limages.Free;
  Inherited;
end;

procedure TPDF2Jpeg.ClearImages;
var
  i: integer;
begin
  for i := 0 to Limages.Count - 1 do
    if Limages.Objects[i] <> nil then
    begin
      Limages.Objects[i].Free;
      Limages.Objects[i] := nil;
    end;
  Limages.Clear;
end;

procedure TPDF2Jpeg.Clear;
begin
  ClearImages;
  globalid := 0;
  reverseorder := False;
end;

function TPDF2Jpeg.OpenPDFFromFile(const fname: string): integer;
begin
  ClearImages;
  Result := AppendPDFFromFile(fname);
end;

function TPDF2Jpeg.AppendPDFFromFile(const fname: string): integer;
var
  data: string;
  fs: TFileStream;
begin
  fs := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(data, fs.Size);
    fs.Read((@data[1])^, fs.Size);
    Result := AppendPDFFromData(data);
    SetLength(data, 0);
  finally
    fs.Free;
  end;
end;

function TPDF2Jpeg.OpenPDFFromData(const adata: string): integer;
begin
  ClearImages;
  Result := AppendPDFFromData(adata);
end;

function TPDF2Jpeg.AppendPDFFromData(const adata: string): integer;
var
  data: string;
  p, p2, p3: integer;
  chk1, chk2: string;
  jp: TJPEGImage;
  m: TMemoryStream;
  id: integer;
  imgname: string;
  okimage: boolean;
  jok: boolean;
  sz: integer;

  function checkpos(const pp: integer; const len: integer): boolean;
  begin
    if pp <= 0 then
    begin
      Result := False;
      Exit;
    end;

    if pp = 1 then
    else if data[pp - 1] in [#10, #13] then
    else
    begin
      Result := False;
      Exit;
    end;

    if pp + len = Length(data) then
    else if data[pp + len] in [#10, #13] then
    else
    begin
      Result := False;
      Exit;
    end;

    Result := True;
  end;

begin
  Result := 0;

  sz := Length(adata);
  if sz = 0 then
    Exit;

  reverseorder := False;
  p := Pos('CBZ and CBR to PDF Converter', data);
  if p > 0 then
    if p < 2000 then
      reverseorder := True;

  data := adata;
  id := Limages.Count;
  chk1 := 'stream';
  chk2 := 'endstream';

  if Assigned(OnProgress) then
    OnProgress(0.0);
  while True do
  begin
    if Assigned(OnProgress) then
      OnProgress((sz - Length(data)) / sz);

    p := Pos(chk1, data);
    p2 := Pos(chk2, data);
    if p2 < 1 then
      Break;
    if p2 < p then
    begin
      Delete(data, 1, p2 + Length(chk2));
      Continue;
    end;

    p3 := Pos('JFIF', data);
    if p3 > p2 then
    begin
      Delete(data, 1, p2 + Length(chk2));
      Continue;
    end;

    jok := false;
    if not checkpos(p, Length(chk1)) then
    begin
      if p = 0 then
        Break;
      Delete(data, 1, p + Length(chk1) - 1);
      if data = '' then
        Break
      else
      begin
        while data[1] in [#10, #13] do
        begin
          Delete(data, 1, 1);
          if data = '' then
            break;
          p3 := Pos('JFIF', data);
          if p3 = 7 then
            Break;
          if Length(data) <= 10 then
            Continue;
          jok := (Ord(data[1]) = 255) and (Ord(data[2]) = 216);
        end;
        if p3 <> 7 then
          if not jok then
            Continue;
      end
    end;

    if (p3 <> 7) and not jok then
    begin
      Delete(data, 1, p + Length(chk1) - 1);
      while data <> '' do
      begin
        if data[1] in [#10, #13] then
          Delete(data, 1, 1)
        else
          Break;
      end;
    end;

    jok := False;
    if Length(data) > 10 then
      jok := (Ord(data[1]) = 255) and (Ord(data[2]) = 216);

    p := Pos('JFIF', data);
    if p <> 7 then
      if not jok then
        Continue;

    p := Pos(chk2, data);
    if p = 0 then
      Break;

    m := TMemoryStream.Create;
    m.Write((@data[1])^, p);
    m.Position := 0;

    okimage := True;
    jp := TJPEGImage.Create;
    try
      jp.LoadFromStream(m);
    except
      okimage := False;
    end;
    jp.Free;

    if okimage then
    begin
      inc(id);
      imgname := IntToStr(id);
      while Length(imgname) < 4 do
        imgname := '0' + imgname;
      Limages.AddObject(imgname + '.jpg', m);
      inc(Result);
    end
    else
      m.Free;
  end;
  if Assigned(OnProgress) then
    OnProgress(1.0);
end;

procedure TPDF2Jpeg.SaveImagesToZip(const fname: string);
var
  pk3: TWZipFile;
  i: integer;
begin
  pk3 := TWZipFile.Create;
  if Assigned(OnProgress) then
    OnProgress(0.0);
  for i := 0 to Limages.Count - 1 do
  begin
    AddStreamToZip(pk3, Limages.Strings[i], Limages.Objects[i] as TMemoryStream);
    if Assigned(OnProgress) then
      OnProgress(i / Limages.Count);
  end;
  pk3.SaveToFile(fname);
  pk3.Free;
  if Assigned(OnProgress) then
    OnProgress(1.0);
end;

procedure TPDF2Jpeg.SaveImagesToDir(const apath: string);
var
  i: integer;
  path: string;
begin
  if Assigned(OnProgress) then
    OnProgress(0.0);
  path := Trim(apath);
  if path <> '' then
    if path[Length(path)] <> '\' then
      path := path + '\';
  if not DirectoryExists(path) then
    ForceDirectories(path);
  for i := 0 to Limages.Count - 1 do
  begin
    (Limages.Objects[i] as TMemoryStream).SaveToFile(path + Limages.Strings[i]);
    if Assigned(OnProgress) then
      OnProgress(i / Limages.Count);
  end;
  if Assigned(OnProgress) then
    OnProgress(1.0);
end;

procedure TPDF2Jpeg.SaveImageTo(const idx: integer; const apath: string);
begin
  if (idx < 0) or (idx >= Limages.Count) then
    Exit;
  (Limages.Objects[idx] as TMemoryStream).SaveToFile(apath);
end;

function TPDF2Jpeg.Count: integer;
begin
  Result := Limages.Count;
end;

function TPDF2Jpeg.GetNewJPEGFromIndex(const idx: integer): TJPEGImage;
var
  m: TMemoryStream;
begin
  if (idx < 0) or (idx >= Limages.Count) then
  begin
    Result := nil;
    Exit;
  end;
  m := Limages.Objects[idx] as TMemoryStream;
  m.Position := 0;
  Result := TJPEGImage.Create;
  Result.LoadFromStream(m);
end;

procedure PDF2ZIP(const pname, zname: string);
var
  pdf: TPDF2Jpeg;
begin
  pdf := TPDF2Jpeg.Create;
  pdf.AppendPDFFromFile(pname);
  pdf.SaveImagesToZip(zname);
  pdf.Free;
end;

end.