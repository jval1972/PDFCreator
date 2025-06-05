unit img_utils;

interface

uses
  Windows, Graphics, FileCtrl, SysUtils, Classes, JPEG, pngimage;

type
  TRGBArray = array[Word] of TRGBTriple;
  pRGBArray = ^TRGBArray;

procedure ResizeJPGImage(FileName, FileOut: string; MaxWidth: Integer; Ql: Integer);

procedure cropjpeg(var j: TJPEGImage; const l, t, r, d: integer);

procedure SplitJPG(fn: string);

procedure SplitPNG(fn: string);

function GetStretchRect(const ref: TRect; const w, h: integer): TRect;

function LoadBitmapFromFile(const fname: string): TBitmap;

function SaveBitmapToFile(const bm: TBitmap; const fname: string): boolean;

function RotateImageFile90DegreesCounterClockwise(const finp, fout: string): boolean;

function RotateImageFile90DegreesClockwise(const finp, fout: string): boolean;

function RotateImageFile180Degrees(const finp, fout: string): boolean;

implementation

uses
  options, utils, webp;

procedure SplitJPG(fn: string);
var
  len: integer;
  J, J1, J2: TJPEGImage;
  fn1, fn2: string;
  B, B1, B2: TBitmap;
begin
  fn1 := fn;
  len := Length(fn);
  fn1[len] := ' ';
  fn1[len - 1] := ' ';
  fn1[len - 2] := ' ';
  if fn1[len - 3] <> '.' then
  begin
    fn1[len - 3] := ' ';
    fn1[len - 4] := ' ';
  end
  else
    fn1[len - 3] := ' ';
  fn1 := Trim(fn1);
  fn2 := fn1;
  fn1 := fn1 + '_left.jpg';
  fn2 := fn2 + '_right.jpg';

  B := TBitmap.Create;

  J := TJPEGImage.Create;
  JPG_ReadWebp(J, fn);
  B.Assign(J);
  B.PixelFormat := pf24bit;
  J.Free;

  B1 := TBitmap.Create;
  B1.Width := B.Width div 2;
  B1.Height := B.Height;
  B1.PixelFormat := pf24bit;
  B1.Canvas.Draw(0, 0, B);

  B2 := TBitmap.Create;
  B2.Width := B.Width div 2;
  B2.Height := B.Height;
  B2.PixelFormat := pf24bit;
  B2.Canvas.Draw(-B2.Width, 0, B);

  B.Free;

  J1 := TJPEGImage.Create;
  J1.CompressionQuality := optjpegcompression;
  J1.Assign(B1);
  J1.SaveToFile(fn1);
  J1.Free;
  B1.Free;

  J2 := TJPEGImage.Create;
  J2.CompressionQuality := optjpegcompression;
  J2.Assign(B2);
  J2.SaveToFile(fn2);
  J2.Free;
  B2.Free;
end;

procedure SplitPNG(fn: string);
var
  len: integer;
  P: TPngObject;
  J1, J2: TJPEGImage;
  fn1, fn2: string;
  B, B1, B2: TBitmap;
begin
  fn1 := fn;
  len := Length(fn);
  fn1[len] := ' ';
  fn1[len - 1] := ' ';
  fn1[len - 2] := ' ';
  if fn1[len - 3] <> '.' then
  begin
    fn1[len - 3] := ' ';
    fn1[len - 4] := ' ';
  end
  else
    fn1[len - 3] := ' ';
  fn1 := Trim(fn1);
  fn2 := fn1;
  fn1 := fn1 + '_left.jpg';
  fn2 := fn2 + '_right.jpg';

  B := TBitmap.Create;

  P := TPngObject.Create;
  PNG_ReadWebp(P, fn);
  B.Assign(P);
  B.PixelFormat := pf24bit;
  P.Free;

  B1 := TBitmap.Create;
  B1.Width := B.Width div 2;
  B1.Height := B.Height;
  B1.PixelFormat := pf24bit;
  B1.Canvas.Draw(0, 0, B);

  B2 := TBitmap.Create;
  B2.Width := B.Width div 2;
  B2.Height := B.Height;
  B2.PixelFormat := pf24bit;
  B2.Canvas.Draw(-B2.Width, 0, B);

  B.Free;

  J1 := TJPEGImage.Create;
  J1.CompressionQuality := optjpegcompression;
  J1.Assign(B1);
  J1.SaveToFile(fn1);
  J1.Free;
  B1.Free;

  J2 := TJPEGImage.Create;
  J2.CompressionQuality := optjpegcompression;
  J2.Assign(B2);
  J2.SaveToFile(fn2);
  J2.Free;
  B2.Free;
end;

{---------------------------------------------------------------------------
-----------------------}

procedure SmoothResize(Src, Dst: TBitmap);
var
  x, y: Integer;
  xP, yP: Integer;
  xP2, yP2: Integer;
  SrcLine1, SrcLine2: pRGBArray;
  t3: Integer;
  z, z2, iz2: Integer;
  DstLine: pRGBArray;
  DstGap: Integer;
  w1, w2, w3, w4: Integer;
begin
  Src.PixelFormat := pf24Bit;
  Dst.PixelFormat := pf24Bit;

  if (Src.Width = Dst.Width) and (Src.Height = Dst.Height) then
    Dst.Assign(Src)
  else
  begin
    DstLine := Dst.ScanLine[0];
    DstGap  := Integer(Dst.ScanLine[1]) - Integer(DstLine);

    xP2 := MulDiv(pred(Src.Width), $10000, Dst.Width);
    yP2 := MulDiv(pred(Src.Height), $10000, Dst.Height);
    yP  := 0;

    for y := 0 to pred(Dst.Height) do
    begin
      xP := 0;

      SrcLine1 := Src.ScanLine[yP shr 16];

      if (yP shr 16 < pred(Src.Height)) then
        SrcLine2 := Src.ScanLine[succ(yP shr 16)]
      else
        SrcLine2 := Src.ScanLine[yP shr 16];

      z2  := succ(yP and $FFFF);
      iz2 := succ((not yp) and $FFFF);
      for x := 0 to pred(Dst.Width) do
      begin
        t3 := xP shr 16;
        z  := xP and $FFFF;
        w2 := MulDiv(z, iz2, $10000);
        w1 := iz2 - w2;
        w4 := MulDiv(z, z2, $10000);
        w3 := z2 - w4;
        DstLine[x].rgbtRed := (SrcLine1[t3].rgbtRed * w1 +
          SrcLine1[t3 + 1].rgbtRed * w2 +
          SrcLine2[t3].rgbtRed * w3 + SrcLine2[t3 + 1].rgbtRed * w4) shr 16;
        DstLine[x].rgbtGreen :=
          (SrcLine1[t3].rgbtGreen * w1 + SrcLine1[t3 + 1].rgbtGreen * w2 +

          SrcLine2[t3].rgbtGreen * w3 + SrcLine2[t3 + 1].rgbtGreen * w4) shr 16;
        DstLine[x].rgbtBlue := (SrcLine1[t3].rgbtBlue * w1 +
          SrcLine1[t3 + 1].rgbtBlue * w2 +
          SrcLine2[t3].rgbtBlue * w3 +
          SrcLine2[t3 + 1].rgbtBlue * w4) shr 16;
        Inc(xP, xP2);
      end; {for}
      Inc(yP, yP2);
      DstLine := pRGBArray(Integer(DstLine) + DstGap);
    end; {for}
  end; {if}
end; {SmoothResize}

{---------------------------------------------------------------------------
-----------------------}

function LoadJPEGPictureFile(Bitmap: TBitmap; FilePath, FileName: string): Boolean;
var
  JPEGImage: TJPEGImage;
begin
  Result := False;
  if FileName <> '' then
  begin
    try  // Start of try except
      JPEGImage := TJPEGImage.Create;  // Create the JPEG image... try  // now

      try  // to load the file but
        JPG_ReadWebp(JPEGImage, FilePath + FileName);
        // might fail...with an Exception.
        Bitmap.Assign(JPEGImage);
        Result := True;
        // Assign the image to our bitmap.Result := True;
        // Got it so return True.
      finally
        JPEGImage.Free;  // ...must get rid of the JPEG image. finally
      end; {try}
    except
      Result := False; // Oops...never Loaded, so return False.
    end; {try}
  end; {if}
end; {LoadJPEGPictureFile}


{---------------------------------------------------------------------------
-----------------------}


function SaveJPEGPictureFile(Bitmap: TBitmap; FilePath, FileName: string;
  Quality: Integer): Boolean;
begin
  Result := False;
  try
    if DirectoryExists(FilePath) then
    begin
      with TJPegImage.Create do
      begin
        try
          Assign(Bitmap);
          CompressionQuality := Quality;
          SaveToFile(FilePath + FileName);
          Result := True;
        finally
          Free;
        end; {try}
      end; {with}
    end; {if}
  except
    raise;
  end; {try}
end; {SaveJPEGPictureFile}


{---------------------------------------------------------------------------
-----------------------}

procedure ResizeJPGImage(FileName, FileOut: string; MaxWidth: Integer; Ql: Integer);
var
  OldBitmap: TBitmap;
  NewBitmap: TBitmap;
  aWidth: Integer;
begin
  OldBitmap := TBitmap.Create;
  try
    if LoadJPEGPictureFile(OldBitmap, ExtractFilePath(FileName),
      ExtractFileName(FileName)) then
    begin
      aWidth := OldBitmap.Width;
      if (aWidth > MaxWidth) then
      begin
        NewBitmap := TBitmap.Create;
        try
          NewBitmap.Width := MaxWidth;
          NewBitmap.Height := MulDiv(MaxWidth, OldBitmap.Height, aWidth);
          SmoothResize(OldBitmap, NewBitmap);
          SaveJPEGPictureFile(NewBitmap, ExtractFilePath(FileName),
            FileOut + '.jpg', Ql);
        finally
          NewBitmap.Free;
        end; {try}
      end
      else
        SaveJPEGPictureFile(OldBitmap, ExtractFilePath(FileName), FileOut + '.jpg', Ql);
    end; {if}
  finally
    OldBitmap.Free;
  end; {try}
end;


{---------------------------------------------------------------------------
-----------------------}

function JPEGDimensions(Filename : string; var X, Y : Word) : boolean;
var
  SegmentPos : Integer;
  SOIcount : Integer;
  b : byte;
begin
  Result  := False;
  with TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone) do
  begin
    try
      Position := 0;
      Read(X, 2);
      if (X <> $D8FF) then
        exit;
      SOIcount  := 0;
      Position  := 0;
      while (Position + 7 < Size) do
      begin
        Read(b, 1);
        if (b = $FF) then begin
          Read(b, 1);
          if (b = $D8) then
            inc(SOIcount);
          if (b = $DA) then
            break;
        end; {if}
      end; {while}
      if (b <> $DA) then
        exit;
      SegmentPos  := -1;
      Position    := 0;
      while (Position + 7 < Size) do
      begin
        Read(b, 1);
        if (b = $FF) then
        begin
          Read(b, 1);
          if (b in [$C0, $C1, $C2]) then
          begin
            SegmentPos  := Position;
            dec(SOIcount);
            if (SOIcount = 0) then
              break;
          end; {if}
        end; {if}
      end; {while}
      if (SegmentPos = -1) then
        exit;
      if (Position + 7 > Size) then
        exit;
      Position := SegmentPos + 3;
      Read(Y, 2);
      Read(X, 2);
      X := Swap(X);
      Y := Swap(Y);
      Result  := true;
    finally
      Free;
    end; {try}
  end; {with}
end; {JPEGDimensions}

procedure cropjpeg(var j: TJPEGImage; const l, t, r, d: integer);
var
  NB: TBitmap;
begin
  NB := TBitmap.Create;
  NB.Width := r - l;
  NB.Height := d - t;
  NB.PixelFormat := pf24bit;
  NB.Canvas.Draw(-l, -t, j);
  j.Assign(NB);
  NB.Free;
end;

function GetStretchRect(const ref: TRect; const w, h: integer): TRect;
var
  rw, rh: integer;
  l: Integer;
begin
  rw := ref.Right - ref.Left;
  rh := ref.Bottom - ref.Top;

  ZeroMemory(@Result, SizeOf(Result));
  if (rw = 0) or (rh = 0) then
    Exit;
  if (w = 0) or (h = 0) then
    Exit;

  if (w / rw) > (h / rh) then
  begin
    Result.Left := ref.Left;
    Result.Right := ref.Right;
    l := Round((rw / w) * rh);
    Result.Top := ref.Top + (rh - l) div 2;
    Result.Bottom := Result.Top + l;
  end
  else
  begin
    Result.Top := ref.Top;
    Result.Bottom := ref.Bottom;
    l := Round((rh / h) * w);
    Result.Left := ref.Left + (rw - l) div 2;
    Result.Right := Result.Left + l;
  end;
end;

procedure RotateBitmap90DegreesCounterClockwise(var ABitmap: TBitmap);
const
  BitsPerByte = 8;
var
  PbmpInfoR: PBitmapInfoHeader;
  bmpBuffer, bmpBufferR: PByte;
  MemoryStream, MemoryStreamR: TMemoryStream;
  PbmpBuffer, PbmpBufferR: PByte;
  BytesPerPixel, PixelsPerByte: LongInt;
  BytesPerScanLine, BytesPerScanLineR: LongInt;
  PaddingBytes: LongInt;
  BitmapOffset: LongInt;
  BitCount: LongInt;
  WholeBytes, ExtraPixels: LongInt;
  SignificantBytes, SignificantBytesR: LongInt;
  ColumnBytes: LongInt;
  AtLeastEightBitColor: Boolean;
  T: LongInt;

  procedure NonIntegralByteRotate;
  var
    X, Y: LongInt;
    I: LongInt;
    MaskBits, CurrentBits: Byte;
    FirstMask, LastMask: Byte;
    PFirstScanLine: PByte;
    FirstIndex, CurrentBitIndex: LongInt;
    ShiftRightAmount, ShiftRightStart: LongInt;
  begin
    Inc(PbmpBuffer, BytesPerScanLine * (PbmpInfoR^.biHeight - 1) );
    PFirstScanLine := bmpBufferR;
    FirstIndex := BitsPerByte - BitCount;
    LastMask := 1 shl BitCount - 1;
    FirstMask := LastMask shl FirstIndex;
    CurrentBits := FirstMask;
    CurrentBitIndex := FirstIndex;
    ShiftRightStart := BitCount * (PixelsPerByte - 1);
    for Y := 1 to PbmpInfoR^.biHeight do
    begin
      PbmpBufferR := PFirstScanLine;
      for X := 1 to WholeBytes do
      begin
        MaskBits := FirstMask;
        ShiftRightAmount := ShiftRightStart;
        for I := 1 to PixelsPerByte do
        begin
          PbmpBufferR^ := ( PbmpBufferR^ and not CurrentBits ) or ((PbmpBuffer^ and MaskBits) shr ShiftRightAmount shl CurrentBitIndex);
          MaskBits := MaskBits shr BitCount;
          Inc(PbmpBufferR, BytesPerScanLineR);
          Dec(ShiftRightAmount, BitCount);
        end;
        Inc(PbmpBuffer);
      end;
      if ExtraPixels <> 0 then
      begin
        MaskBits := FirstMask;
        ShiftRightAmount := ShiftRightStart;
        for I := 1 to ExtraPixels do
        begin
          PbmpBufferR^ := ( PbmpBufferR^ and not CurrentBits ) or ((PbmpBuffer^ and MaskBits) shr ShiftRightAmount shl CurrentBitIndex);
          MaskBits := MaskBits shr BitCount;
          Inc(PbmpBufferR, BytesPerScanLineR);
          Dec(ShiftRightAmount, BitCount);
        end;
        Inc(PbmpBuffer);
      end;
      Inc(PbmpBuffer, PaddingBytes);
      Dec(PbmpBuffer, BytesPerScanLine shl 1);
      if CurrentBits = LastMask then
      begin
        CurrentBits := FirstMask;
        CurrentBitIndex := FirstIndex;
        Inc(PFirstScanLine);
      end
      else
      begin
        CurrentBits := CurrentBits shr BitCount;
        Dec(CurrentBitIndex, BitCount);
      end;
    end;
  end;

  procedure IntegralByteRotate;
  var
    X, Y: LongInt;
  begin
    Inc(PbmpBufferR, SignificantBytesR - BytesPerPixel);
    for Y := 1 to PbmpInfoR^.biHeight do
    begin
      for X := 1 to PbmpInfoR^.biWidth do
      begin
        Move(PbmpBuffer^, PbmpBufferR^, BytesPerPixel);

        Inc(PbmpBuffer, BytesPerPixel);

        Inc(PbmpBufferR, BytesPerScanLineR);
      end;
      Inc(PbmpBuffer, PaddingBytes);
      Dec(PbmpBufferR, ColumnBytes + BytesPerPixel);
    end;
  end;

begin
  MemoryStream := TMemoryStream.Create;
  ABitmap.SaveToStream(MemoryStream);
  ABitmap.Free;
  bmpBuffer := MemoryStream.Memory;
  BitmapOffset := PBitmapFileHeader(bmpBuffer)^.bfOffBits;
  Inc(bmpBuffer, SizeOf(TBitmapFileHeader));
  PbmpInfoR := PBitmapInfoHeader(bmpBuffer);
  bmpBuffer := MemoryStream.Memory;
  Inc(bmpBuffer, BitmapOffset);
  PbmpBuffer := bmpBuffer;
  with PbmpInfoR^ do
  begin
    BitCount := biBitCount;
    BytesPerScanLine := ((((biWidth * BitCount) + 31) div 32) * SizeOf(DWORD));
    BytesPerScanLineR := ((((biHeight * BitCount) + 31) div 32) * SizeOf(DWORD));
    AtLeastEightBitColor := BitCount >= BitsPerByte;
    if AtLeastEightBitColor then
    begin
      BytesPerPixel := biBitCount shr 3;
      SignificantBytes := biWidth * BitCount shr 3;
      SignificantBytesR := biHeight * BitCount shr 3;
      PaddingBytes := BytesPerScanLine - SignificantBytes;
      ColumnBytes := BytesPerScanLineR * biWidth;
    end
    else
    begin
      PixelsPerByte := SizeOf(Byte) * BitsPerByte div BitCount;
      WholeBytes := biWidth div PixelsPerByte;
      ExtraPixels := biWidth mod PixelsPerByte;
      PaddingBytes := BytesPerScanLine - WholeBytes;
      if ExtraPixels <> 0 then Dec(PaddingBytes);
    end;
    MemoryStreamR := TMemoryStream.Create;
    MemoryStreamR.SetSize(BitmapOffset + BytesPerScanLineR * biWidth);
  end;
  MemoryStream.Seek(0, soFromBeginning);
  MemoryStreamR.CopyFrom(MemoryStream, BitmapOffset);
  bmpBufferR := MemoryStreamR.Memory;
  Inc(bmpBufferR, BitmapOffset);
  PbmpBufferR := bmpBufferR;
  if AtLeastEightBitColor then
    IntegralByteRotate
  else
    NonIntegralByteRotate;
  MemoryStream.Free;
  PbmpBufferR := MemoryStreamR.Memory;
  Inc( PbmpBufferR, SizeOf(TBitmapFileHeader) );
  PbmpInfoR := PBitmapInfoHeader(PbmpBufferR);
  with PbmpInfoR^ do
  begin
    T := biHeight;
    biHeight := biWidth;
    biWidth := T;
    biSizeImage := 0;
  end;
  ABitmap := TBitmap.Create;
  MemoryStreamR.Seek(0, soFromBeginning);
  ABitmap.LoadFromStream(MemoryStreamR);
  MemoryStreamR.Free;
end;

procedure RotateBitmap90DegreesClockwise(var ABitmap: TBitmap);
const
  BitsPerByte = 8;
var
  PbmpInfoR: PBitmapInfoHeader;
  bmpBuffer, bmpBufferR: PByte;
  MemoryStream, MemoryStreamR: TMemoryStream;
  PbmpBuffer, PbmpBufferR: PByte;
  BytesPerPixel, PixelsPerByte: LongInt;
  BytesPerScanLine, BytesPerScanLineR: LongInt;
  PaddingBytes: LongInt;
  BitmapOffset: LongInt;
  BitCount: LongInt;
  WholeBytes, ExtraPixels: LongInt;
  SignificantBytes: LongInt;
  ColumnBytes: LongInt;
  AtLeastEightBitColor: Boolean;
  T: LongInt;

  procedure NonIntegralByteRotate;
  var
    X, Y: LongInt;
    I: LongInt;
    MaskBits, CurrentBits: Byte;
    FirstMask, LastMask: Byte;
    PLastScanLine: PByte;
    FirstIndex, CurrentBitIndex: LongInt;
    ShiftRightAmount, ShiftRightStart: LongInt;
  begin
    PLastScanLine := bmpBufferR;
    Inc(PLastScanLine, BytesPerScanLineR * (PbmpInfoR^.biWidth - 1) );
    FirstIndex := BitsPerByte - BitCount;
    LastMask := 1 shl BitCount - 1;
    FirstMask := LastMask shl FirstIndex;
    CurrentBits := FirstMask;
    CurrentBitIndex := FirstIndex;
    ShiftRightStart := BitCount * (PixelsPerByte - 1);
    for Y := 1 to PbmpInfoR^.biHeight do
    begin
      PbmpBufferR := PLastScanLine;
      for X := 1 to WholeBytes do
      begin
        MaskBits := FirstMask;
        ShiftRightAmount := ShiftRightStart;
        for I := 1 to PixelsPerByte do
        begin
          PbmpBufferR^ := ( PbmpBufferR^ and not CurrentBits ) or ((PbmpBuffer^ and MaskBits) shr ShiftRightAmount shl CurrentBitIndex);
          MaskBits := MaskBits shr BitCount;
          Dec(PbmpBufferR, BytesPerScanLineR);
          Dec(ShiftRightAmount, BitCount);
        end;
        Inc(PbmpBuffer);
      end;
      if ExtraPixels <> 0 then
      begin
        MaskBits := FirstMask;
        ShiftRightAmount := ShiftRightStart;
        for I := 1 to ExtraPixels do
        begin
          PbmpBufferR^ := (PbmpBufferR^ and not CurrentBits) or ((PbmpBuffer^ and MaskBits) shr ShiftRightAmount shl CurrentBitIndex);
          MaskBits := MaskBits shr BitCount;
          Dec(PbmpBufferR, BytesPerScanLineR);
          Dec(ShiftRightAmount, BitCount);
        end;
        Inc(PbmpBuffer);
      end;
      Inc(PbmpBuffer, PaddingBytes);
      if CurrentBits = LastMask then
      begin
        CurrentBits := FirstMask;
        CurrentBitIndex := FirstIndex;
        Inc(PLastScanLine);
      end
      else
      begin
        CurrentBits := CurrentBits shr BitCount;
        Dec(CurrentBitIndex, BitCount);
      end;
    end;
  end;

  procedure IntegralByteRotate;
  var
    X, Y: LongInt;
  begin
    Inc(PbmpBufferR, BytesPerScanLineR * (PbmpInfoR^.biWidth - 1));
    for Y := 1 to PbmpInfoR^.biHeight do
    begin
      for X := 1 to PbmpInfoR^.biWidth do
      begin
        Move(PbmpBuffer^, PbmpBufferR^, BytesPerPixel);
        Inc(PbmpBuffer, BytesPerPixel);
        Dec(PbmpBufferR, BytesPerScanLineR);
      end;
      Inc(PbmpBuffer, PaddingBytes);
      Inc(PbmpBufferR, ColumnBytes + BytesPerPixel);
    end;
  end;

begin
  MemoryStream := TMemoryStream.Create;
  ABitmap.SaveToStream(MemoryStream);
  ABitmap.Free;
  bmpBuffer := MemoryStream.Memory;
  BitmapOffset := PBitmapFileHeader(bmpBuffer)^.bfOffBits;
  Inc(bmpBuffer, SizeOf(TBitmapFileHeader));
  PbmpInfoR := PBitmapInfoHeader(bmpBuffer);
  bmpBuffer := MemoryStream.Memory;
  Inc(bmpBuffer, BitmapOffset);
  PbmpBuffer := bmpBuffer;
  with PbmpInfoR^ do
  begin
    BitCount := biBitCount;
    BytesPerScanLine := ((((biWidth * BitCount) + 31) div 32) * SizeOf(DWORD));
    BytesPerScanLineR := ((((biHeight * BitCount) + 31) div 32) * SizeOf(DWORD));
    AtLeastEightBitColor := BitCount >= BitsPerByte;
    if AtLeastEightBitColor then
    begin
      BytesPerPixel := biBitCount shr 3;
      SignificantBytes := biWidth * BitCount shr 3;
      PaddingBytes := BytesPerScanLine - SignificantBytes;
      ColumnBytes := BytesPerScanLineR * biWidth;
    end
    else
    begin
      PixelsPerByte := SizeOf(Byte) * BitsPerByte div BitCount;
      WholeBytes := biWidth div PixelsPerByte;
      ExtraPixels := biWidth mod PixelsPerByte;
      PaddingBytes := BytesPerScanLine - WholeBytes;
      if ExtraPixels <> 0 then Dec(PaddingBytes);
    end;
    MemoryStreamR := TMemoryStream.Create;
    MemoryStreamR.SetSize(BitmapOffset + BytesPerScanLineR * biWidth);
  end;
  MemoryStream.Seek(0, soFromBeginning);
  MemoryStreamR.CopyFrom(MemoryStream, BitmapOffset);
  bmpBufferR := MemoryStreamR.Memory;
  Inc(bmpBufferR, BitmapOffset);
  PbmpBufferR := bmpBufferR;
  if AtLeastEightBitColor then
    IntegralByteRotate
  else
    NonIntegralByteRotate;
  MemoryStream.Free;
  PbmpBufferR := MemoryStreamR.Memory;
  Inc(PbmpBufferR, SizeOf(TBitmapFileHeader));
  PbmpInfoR := PBitmapInfoHeader(PbmpBufferR);
  with PbmpInfoR^ do
  begin
    T := biHeight;
    biHeight := biWidth;
    biWidth := T;
    biSizeImage := 0;
  end;
  ABitmap := TBitmap.Create;
  MemoryStreamR.Seek(0, soFromBeginning);
  ABitmap.LoadFromStream(MemoryStreamR);
  MemoryStreamR.Free;
end;

procedure RotateBitmap180Degrees(var ABitmap: TBitmap);
var
  RotatedBitmap: TBitmap;
begin
  RotatedBitmap := TBitmap.Create;
  with RotatedBitmap do
  begin
    Width := ABitmap.Width;
    Height := ABitmap.Height;
    Canvas.StretchDraw( Rect(ABitmap.Width, ABitmap.Height, 0, 0), ABitmap );
  end;
  ABitmap.Free;
  ABitmap := RotatedBitmap;
end;

procedure RotateJPEG90DegreesCounterClockwise(const jp: TJPEGImage);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  b.Assign(jp);
  RotateBitmap90DegreesCounterClockwise(b);
  jp.Assign(b);
  b.Free;
end;

procedure RotateJPEG90DegreesClockwise(const jp: TJPEGImage);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  b.Assign(jp);
  RotateBitmap90DegreesClockwise(b);
  jp.Assign(b);
  b.Free;
end;

procedure RotateJPEG180Degrees(const jp: TJPEGImage);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  b.Assign(jp);
  RotateBitmap180Degrees(b);
  jp.Assign(b);
  b.Free;
end;

procedure RotatePNG90DegreesCounterClockwise(const pn: TPNGObject);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  b.Assign(pn);
  RotateBitmap90DegreesCounterClockwise(b);
  pn.Assign(b);
  b.Free;
end;

procedure RotatePNG90DegreesClockwise(const pn: TPNGObject);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  b.Assign(pn);
  RotateBitmap90DegreesClockwise(b);
  pn.Assign(b);
  b.Free;
end;

procedure RotatePNG180Degrees(const pn: TPNGObject);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  b.Assign(pn);
  RotateBitmap180Degrees(b);
  pn.Assign(b);
  b.Free;
end;

function RotateBitmapFile90DegreesCounterClockwise(const finp, fout: string): boolean;
var
  b: TBitmap;
begin
  Result := False;
  if not FileExists(finp) then
    Exit;

  b := TBitmap.Create;
  try
    b.LoadFromFile(finp);
    RotateBitmap90DegreesCounterClockwise(b);
    backupfile(fout);
    b.SaveToFile(fout);
    Result := True;
  finally
    b.Free;
  end;
end;

function RotateBitmapFile90DegreesClockwise(const finp, fout: string): boolean;
var
  b: TBitmap;
begin
  Result := False;
  if not FileExists(finp) then
    Exit;

  b := TBitmap.Create;
  try
    b.LoadFromFile(finp);
    RotateBitmap90DegreesClockwise(b);
    b.SaveToFile(fout);
    Result := True;
  finally
    b.Free;
  end;
end;

function RotateBitmapFile180Degrees(const finp, fout: string): boolean;
var
  b: TBitmap;
begin
  Result := False;
  if not FileExists(finp) then
    Exit;

  b := TBitmap.Create;
  try
    b.LoadFromFile(finp);
    RotateBitmap180Degrees(b);
    backupfile(fout);
    b.SaveToFile(fout);
    Result := True;
  finally
    b.Free;
  end;
end;

function RotateJPEGFile90DegreesCounterClockwise(const finp, fout: string): boolean;
var
  jp: TJPEGImage;
begin
  Result := False;
  if not FileExists(finp) then
    Exit;

  jp := TJPEGImage.Create;
  try
    JPG_ReadWebp(jp, finp);
    RotateJPEG90DegreesCounterClockwise(jp);
    jp.CompressionQuality := optjpegcompression;
    backupfile(fout);
    jp.SaveToFile(fout);
    Result := True;
  finally
    jp.Free;
  end;
end;

function RotateJPEGFile90DegreesClockwise(const finp, fout: string): boolean;
var
  jp: TJPEGImage;
begin
  Result := False;
  if not FileExists(finp) then
    Exit;

  jp := TJPEGImage.Create;
  try
    JPG_ReadWebp(jp, finp);
    RotateJPEG90DegreesClockwise(jp);
    jp.CompressionQuality := optjpegcompression;
    backupfile(fout);
    jp.SaveToFile(fout);
    Result := True;
  finally
    jp.Free;
  end;
end;

function RotateJPEGFile180Degrees(const finp, fout: string): boolean;
var
  jp: TJPEGImage;
begin
  Result := False;
  if not FileExists(finp) then
    Exit;

  jp := TJPEGImage.Create;
  try
    JPG_ReadWebp(jp, finp);
    RotateJPEG180Degrees(jp);
    jp.CompressionQuality := optjpegcompression;
    backupfile(fout);
    jp.SaveToFile(fout);
    Result := True;
  finally
    jp.Free;
  end;
end;

function RotatePNGFile90DegreesCounterClockwise(const finp, fout: string): boolean;
var
  pn: TPNGObject;
begin
  Result := False;
  if not FileExists(finp) then
    Exit;

  pn := TPNGObject.Create;
  try
    PNG_ReadWebp(pn, finp);
    RotatePNG90DegreesCounterClockwise(pn);
    backupfile(fout);
    pn.SaveToFile(fout);
    Result := True;
  finally
    pn.Free;
  end;
end;

function RotatePNGFile90DegreesClockwise(const finp, fout: string): boolean;
var
  pn: TPNGObject;
begin
  Result := False;
  if not FileExists(finp) then
    Exit;

  pn := TPNGObject.Create;
  try
    PNG_ReadWebp(pn, finp);
    RotatePNG90DegreesClockwise(pn);
    backupfile(fout);
    pn.SaveToFile(fout);
    Result := True;
  finally
    pn.Free;
  end;
end;

function RotatePNGFile180Degrees(const finp, fout: string): boolean;
var
  pn: TPNGObject;
begin
  Result := False;
  if not FileExists(finp) then
    Exit;

  pn := TPNGObject.Create;
  try
    PNG_ReadWebp(pn, finp);
    RotatePNG180Degrees(pn);
    backupfile(fout);
    pn.SaveToFile(fout);
    Result := True;
  finally
    pn.Free;
  end;
end;

type
  imagetype_t = (it_jpg, it_png, it_bmp, it_unknown);

function ext2typ(const ext: string): imagetype_t;
begin
  if (ext = '.jpg') or (ext = '.jpeg') then
    Result := it_jpg
  else if (ext = '.png') or (ext = '.webp') or (ext = '.tif') or (ext = '.tiff') then
    Result := it_png
  else if ext = '.bmp' then
    Result := it_bmp
  else
    Result := it_unknown;
end;

function LoadBitmapFromFile(const fname: string): TBitmap;
var
  typ: imagetype_t;
  ext: string;
  jp: TJPEGImage;
  pn: TPNGObject;
begin
  ext := LowerCase(ExtractFileExt(fname));
  typ := ext2typ(ext);

  Result := TBitmap.Create;
  try
    if typ = it_jpg then
    begin
      jp := TJPEGImage.Create;
      JPG_ReadWebp(jp, fname);
      Result.Assign(jp);
      jp.Free;
    end
    else if typ = it_png then
    begin
      pn := TPNGObject.Create;
      PNG_ReadWebp(pn, fname);
      Result.Assign(pn);
      pn.Free;
    end
    else
      Result.LoadFromFile(fname);
  except
    Result.Free;
    Result := nil;
  end;
end;

function SaveBitmapToFile(const bm: TBitmap; const fname: string): boolean;
var
  typ: imagetype_t;
  ext: string;
  jp: TJPEGImage;
  pn: TPNGObject;
begin
  ext := LowerCase(ExtractFileExt(fname));
  typ := ext2typ(ext);

  Result := True;
  try
    if typ = it_jpg then
    begin
      jp := TJPEGImage.Create;
      jp.Assign(bm);
      jp.CompressionQuality := optjpegcompression;
      jp.SaveToFile(fname);
      jp.Free;
    end
    else if typ = it_png then
    begin
      pn := TPNGObject.Create;
      pn.Assign(bm);
      pn.SaveToFile(fname);
      pn.Free;
    end
    else
      bm.SaveToFile(fname);
  except
    Result := False;
  end;
end;

function RotateImageFile90DegreesCounterClockwise(const finp, fout: string): boolean;
var
  ext1, ext2: string;
  typ1, typ2: imagetype_t;
  bm: TBitmap;
begin
  ext1 := LowerCase(ExtractFileExt(finp));
  ext2 := LowerCase(ExtractFileExt(fout));
  typ1 := ext2typ(ext1);
  typ2 := ext2typ(ext1);
  if typ1 = typ2 then
  begin
    if typ1 = it_jpg then
      Result := RotateJPEGFile90DegreesCounterClockwise(finp, fout)
    else if typ1 = it_png then
      Result := RotatePNGFile90DegreesCounterClockwise(finp, fout)
    else if typ1 = it_bmp then
      Result := RotateBitmapFile90DegreesCounterClockwise(finp, fout)
    else
      Result := False;
    Exit;
  end;

  if (typ1 = it_unknown) or (typ2 = it_unknown) then
  begin
    Result := False;
    Exit;
  end;

  bm := LoadBitmapFromFile(finp);
  if bm = nil then
  begin
    Result := False;
    Exit;
  end;

  RotateBitmap90DegreesCounterClockwise(bm);

  Result := SaveBitmapToFile(bm, fout);

  bm.Free;
end;

function RotateImageFile90DegreesClockwise(const finp, fout: string): boolean;
var
  ext1, ext2: string;
  typ1, typ2: imagetype_t;
  bm: TBitmap;
begin
  ext1 := LowerCase(ExtractFileExt(finp));
  ext2 := LowerCase(ExtractFileExt(fout));
  typ1 := ext2typ(ext1);
  typ2 := ext2typ(ext2);
  if typ1 = typ2 then
  begin
    if typ1 = it_jpg then
      Result := RotateJPEGFile90DegreesClockwise(finp, fout)
    else if typ1 = it_png then
      Result := RotatePNGFile90DegreesClockwise(finp, fout)
    else if typ1 = it_bmp then
      Result := RotateBitmapFile90DegreesClockwise(finp, fout)
    else
      Result := False;
    Exit;
  end;

  if (typ1 = it_unknown) or (typ2 = it_unknown) then
  begin
    Result := False;
    Exit;
  end;

  bm := LoadBitmapFromFile(finp);
  if bm = nil then
  begin
    Result := False;
    Exit;
  end;

  RotateBitmap90DegreesClockwise(bm);

  Result := SaveBitmapToFile(bm, fout);

  bm.Free;
end;

function RotateImageFile180Degrees(const finp, fout: string): boolean;
var
  ext1, ext2: string;
  typ1, typ2: imagetype_t;
  bm: TBitmap;
begin
  ext1 := LowerCase(ExtractFileExt(finp));
  ext2 := LowerCase(ExtractFileExt(fout));
  typ1 := ext2typ(ext1);
  typ2 := ext2typ(ext1);
  if typ1 = typ2 then
  begin
    if typ1 = it_jpg then
      Result := RotateJPEGFile180Degrees(finp, fout)
    else if typ1 = it_png then
      Result := RotatePNGFile180Degrees(finp, fout)
    else if typ1 = it_bmp then
      Result := RotateBitmapFile180Degrees(finp, fout)
    else
      Result := False;
    Exit;
  end;

  if (typ1 = it_unknown) or (typ2 = it_unknown) then
  begin
    Result := False;
    Exit;
  end;

  bm := LoadBitmapFromFile(finp);
  if bm = nil then
  begin
    Result := False;
    Exit;
  end;

  RotateBitmap180Degrees(bm);

  Result := SaveBitmapToFile(bm, fout);

  bm.Free;
end;

end.
