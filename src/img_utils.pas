unit img_utils;

interface

uses
  Windows, Graphics, FileCtrl, SysUtils, Classes, JPEG, pngimage;

type
  TRGBArray = array[Word] of TRGBTriple;
  pRGBArray = ^TRGBArray;

procedure ResizeImage(FileName, FileOut: string; MaxWidth: Integer; Ql: Integer);

procedure cropjpeg(var j: TJPEGImage; const l, t, r, d: integer);

procedure SplitJPG(fn: string);

procedure SplitPNG(fn: string);

function GetStretchRect(const ref: TRect; const w, h: integer): TRect;

implementation

uses
  options, webp;

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

procedure ResizeImage(FileName, FileOut: string; MaxWidth: Integer; Ql: Integer);
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

end.
