unit webpimage;

interface

uses
  Classes, jpeg;

type
  TWEBPImage = class(TJPEGImage)
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

implementation

uses
  libwebp, webp, Graphics;

type
  TByteArray = array[0..$FFFF] of byte;
  PByteArray = ^TByteArray;

procedure TWEBPImage.LoadFromStream(Stream: TStream);
var
  A: array[0..14] of byte;
  buffer, data, line: PByteArray;
  sz: integer;
  w, h: integer;
  b: TBitmap;
  i, j: Integer;
  p: Int64;
begin
  p := Stream.Position;
  sz := Stream.Size - p;
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
      stream.Position := p;
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
      Assign(b);
      b.Free;
      Exit;
    end;
  end;
  stream.Position := p;

  inherited LoadFromStream(Stream);
end;

initialization
  { register the TGifBitmap as a new graphic file format
    now all the TPicture storage stuff can access our new
    GIF graphic format !
  }
  TPicture.RegisterFileFormat('webp', 'WEBP Image', TWEBPImage);

finalization
  TPicture.UnregisterGraphicClass(TWEBPImage);

end.
 