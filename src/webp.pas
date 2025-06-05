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

  TIFF_BIGENDIAN: array[0..3] of Byte = (
    $4D, $4D, $00, $2A
  );

  TIFF_LITTLEENDIAN: array[0..3] of Byte = (
    $49, $49, $2A, $00
  );

implementation

uses
  libwebp, xTIFF;

type
  TByteArray = array[0..$FFFF] of byte;
  PByteArray = ^TByteArray;

function PNG_ReadWebp(const PNG: TPNGObject; fname: string): boolean;
var
  A1: array[0..14] of byte;
  A2: array[0..3] of byte;
  buffer, data, line: PByteArray;
  sz: integer;
  w, h: integer;
  b: TBitmap;
  tiff: TTIFFBitmap;
  i, j: Integer;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(fname, fmOpenRead);

  sz := Stream.Size;
  if sz > 14 then
  begin
    Stream.Read(A1, SizeOf(A1));
    if (A1[0] = WEBP_HEADER[0]) and
       (A1[1] = WEBP_HEADER[1]) and
       (A1[2] = WEBP_HEADER[2]) and
       (A1[3] = WEBP_HEADER[3]) and
       (A1[8] = WEBP_HEADER[8]) and
       (A1[9] = WEBP_HEADER[9]) and
       (A1[10] = WEBP_HEADER[10]) and
       (A1[11] = WEBP_HEADER[11]) and
       (A1[12] = WEBP_HEADER[12]) and
       (A1[13] = WEBP_HEADER[13]) and
       (A1[14] = WEBP_HEADER[14]) then
    begin
      Stream.Position := 0;
      GetMem(buffer, sz);
      Stream.Read(buffer^, sz);
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

  if sz > 4 then
  begin
    Stream.Position := 0;
    Stream.Read(A2, SizeOf(A2));
    if ((A2[0] = TIFF_BIGENDIAN[0]) and
        (A2[1] = TIFF_BIGENDIAN[1]) and
        (A2[2] = TIFF_BIGENDIAN[2]) and
        (A2[3] = TIFF_BIGENDIAN[3])) or
       ((A2[0] = TIFF_LITTLEENDIAN[0]) and
        (A2[1] = TIFF_LITTLEENDIAN[1]) and
        (A2[2] = TIFF_LITTLEENDIAN[2]) and
        (A2[3] = TIFF_LITTLEENDIAN[3])) then
    begin
      Stream.Position := 0;
      tiff := TTIFFBitmap.Create;
      try
        tiff.LoadFromStream(Stream);
        PNG.Assign(tiff);
      finally
        tiff.Free;
      end;

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
  A1: array[0..14] of byte;
  A2: array[0..3] of byte;
  buffer, data, line: PByteArray;
  sz: integer;
  w, h: integer;
  b: TBitmap;
  tiff: TTIFFBitmap;
  i, j: Integer;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(fname, fmOpenRead);

  sz := Stream.Size;
  if sz > 14 then
  begin
    Stream.Read(A1, SizeOf(A1));
    if (A1[0] = WEBP_HEADER[0]) and
       (A1[1] = WEBP_HEADER[1]) and
       (A1[2] = WEBP_HEADER[2]) and
       (A1[3] = WEBP_HEADER[3]) and
       (A1[8] = WEBP_HEADER[8]) and
       (A1[9] = WEBP_HEADER[9]) and
       (A1[10] = WEBP_HEADER[10]) and
       (A1[11] = WEBP_HEADER[11]) and
       (A1[12] = WEBP_HEADER[12]) and
       (A1[13] = WEBP_HEADER[13]) and
       (A1[14] = WEBP_HEADER[14]) then
    begin
      Stream.Position := 0;
      GetMem(buffer, sz);
      Stream.Read(buffer^, sz);
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

  if sz > 4 then
  begin
    Stream.Position := 0;
    Stream.Read(A2, SizeOf(A2));
    if ((A2[0] = TIFF_BIGENDIAN[0]) and
        (A2[1] = TIFF_BIGENDIAN[1]) and
        (A2[2] = TIFF_BIGENDIAN[2]) and
        (A2[3] = TIFF_BIGENDIAN[3])) or
       ((A2[0] = TIFF_LITTLEENDIAN[0]) and
        (A2[1] = TIFF_LITTLEENDIAN[1]) and
        (A2[2] = TIFF_LITTLEENDIAN[2]) and
        (A2[3] = TIFF_LITTLEENDIAN[3])) then
    begin
      Stream.Position := 0;
      tiff := TTIFFBitmap.Create;
      try
        tiff.LoadFromStream(Stream);
        JPG.Assign(tiff);
      finally
        tiff.Free;
      end;

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
