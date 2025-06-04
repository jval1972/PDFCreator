unit webp;

interface

uses
  Classes,
  Graphics,
  SysUtils,
  pngimage,
  jpeg;

function PNG_ReadWebp(const PNG: TPNGObject; fname: string): boolean;

function JPG_ReadWebp(const JPG: TJPEGImage; fname: string): boolean;

function IsWebpImage(const fname: string): boolean;

const
  WEBP_HEADER: array[0..14] of Byte = (
    $52, $49, $46, $46, $00, $17, $03, $00, $57, $45, $42, $50, $56, $50, $38
  );

implementation

uses
  libwebp;

type
  TByteArray = array[0..$FFFF] of byte;
  PByteArray = ^TByteArray;

function PNG_ReadWebp(const PNG: TPNGObject; fname: string): boolean;
var
  A: array[0..14] of byte;
  buffer, data, line: PByteArray;
  sz: integer;
  w, h: integer;
  b: TBitmap;
  i, j: Integer;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(fname, fmOpenRead);

  sz := Stream.Size;
  if sz > 14 then
  begin
    Stream.Read(A, SizeOf(A));
    if (A[0] = WEBP_HEADER[0]) and
       (A[1] = WEBP_HEADER[1]) and
       (A[2] = WEBP_HEADER[2]) and
       (A[3] = WEBP_HEADER[3]) and
       (A[8] = WEBP_HEADER[8]) and
       (A[9] = WEBP_HEADER[9]) and
       (A[10] = WEBP_HEADER[10]) and
       (A[11] = WEBP_HEADER[11]) and
       (A[12] = WEBP_HEADER[12]) and
       (A[13] = WEBP_HEADER[13]) and
       (A[14] = WEBP_HEADER[14]) then
    begin
      stream.Position := 0;
      GetMem(buffer, sz);
      stream.Read(buffer^, sz);
      data := PByteArray(WebPDecodeBGRA(@buffer[0], sz, @w, @h));
      // Free buffer
      FreeMem(buffer, sz);
      // Load to image
      b := TBitmap.Create;
      b.Width := w;
      b.Height := h;
      b.PixelFormat := pf32bit;
      for j := 0 to h - 1 do
      begin
        line := b.ScanLine[j];
        for i := 0 to 4 * w - 1 do
          line[i] := data[j * 4 * w + i];
      end;
      WebPFree(data);
      PNG.Assign(b);
      b.Free;
      Stream.Free;
      Result := True;
      Exit;
    end;
  end;
  Stream.Free;

  Result := False;
  PNG.LoadFromFile(fname);
end;

function JPG_ReadWebp(const JPG: TJPEGImage; fname: string): boolean;
var
  A: array[0..14] of byte;
  buffer, data, line: PByteArray;
  sz: integer;
  w, h: integer;
  b: TBitmap;
  i, j: Integer;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(fname, fmOpenRead);

  sz := Stream.Size;
  if sz > 14 then
  begin
    Stream.Read(A, SizeOf(A));
    if (A[0] = WEBP_HEADER[0]) and
       (A[1] = WEBP_HEADER[1]) and
       (A[2] = WEBP_HEADER[2]) and
       (A[3] = WEBP_HEADER[3]) and
       (A[8] = WEBP_HEADER[8]) and
       (A[9] = WEBP_HEADER[9]) and
       (A[10] = WEBP_HEADER[10]) and
       (A[11] = WEBP_HEADER[11]) and
       (A[12] = WEBP_HEADER[12]) and
       (A[13] = WEBP_HEADER[13]) and
       (A[14] = WEBP_HEADER[14]) then
    begin
      stream.Position := 0;
      GetMem(buffer, sz);
      stream.Read(buffer^, sz);
      data := PByteArray(WebPDecodeBGRA(@buffer[0], sz, @w, @h));
      // Free buffer
      FreeMem(buffer, sz);
      // Load to image
      b := TBitmap.Create;
      b.Width := w;
      b.Height := h;
      b.PixelFormat := pf32bit;
      for j := 0 to h - 1 do
      begin
        line := b.ScanLine[j];
        for i := 0 to 4 * w - 1 do
          line[i] := data[j * 4 * w + i];
      end;
      WebPFree(data);
      JPG.Assign(b);
      b.Free;
      Stream.Free;
      Result := True;
      Exit;
    end;
  end;
  Stream.Free;

  Result := False;
  JPG.LoadFromFile(fname);
end;

function IsWebpImage(const fname: string): boolean;
var
  A: array[0..14] of byte;
  sz: Integer;
  Stream: TFileStream;
begin
  Result := False;

  Stream := TFileStream.Create(fname, fmOpenRead);

  sz := Stream.Size;
  if sz > 14 then
  begin
    Stream.Read(A, SizeOf(A));
    if (A[0] = WEBP_HEADER[0]) and
       (A[1] = WEBP_HEADER[1]) and
       (A[2] = WEBP_HEADER[2]) and
       (A[3] = WEBP_HEADER[3]) and
       (A[8] = WEBP_HEADER[8]) and
       (A[9] = WEBP_HEADER[9]) and
       (A[10] = WEBP_HEADER[10]) and
       (A[11] = WEBP_HEADER[11]) and
       (A[12] = WEBP_HEADER[12]) and
       (A[13] = WEBP_HEADER[13]) and
       (A[14] = WEBP_HEADER[14]) then
      Result := True;
  end;
  Stream.Free;
end;

end.
