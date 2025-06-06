unit pdfexport;

interface

uses
  SysUtils, Classes;

procedure MakePDF(const folder: string; const l: TStringList; const fname: string);

procedure MakePDFEx(const folder: string; const l: TStringList; const fname: string);

implementation

uses
  StrUtils, SynPdf, webp, img_utils, img_names, options, jpeg, pngimage;

procedure MakePDF(const folder: string; const l: TStringList; const fname: string);
var
  obPDF: TPdfDocument;
  obPage: TPdfPage;
  pdfimage: TPdfImage;
  JPG: TJPEGImage;
  i: integer;
  w, h: integer;
  sname: string;
  m: TMemoryStream;
  PNG: TPNGObject;
begin
  if l.Count = 0 then
    Exit;

  if RightStr(UpperCase(l[0]), 4) = '.PNG' then
  begin
    PNG := TPNGObject.Create;
    try
      PNG.LoadFromFile(folder + l[0]);
      w := PNG.Width;
      h := PNG.Height;
    finally
      PNG.Free;
    end;
  end
  else
  begin
    JPG := TJPEGImage.Create;
    try
      JPG_ReadWebp(JPG, folder + l[0]);
      w := JPG.Width;
      h := JPG.Height;
    finally
      JPG.Free;
    end;
  end;

  obPDF := TPdfDocument.Create(false, 0, false);
  obPDF.GeneratePDF15File := true;
  obPDF.DefaultPaperSize := psUserDefined;
  obPDF.DefaultPageWidth := w;
  obPDF.DefaultPageHeight := h;
  obPDF.DefaultPageLandscape := false;
  obPDF.CompressionMethod := cmNone;
  obPDF.ForceJPEGCompression := 0;
  obPDF.Info.Creator := 'PDFCreator';

  m := TMemoryStream.Create;
  for i := 0 to l.Count - 1 do
    if FileExists(folder + l[i]) then
    begin
      if RightStr(UpperCase(l[i]), 4) = '.PNG' then
      begin
        PNG := TPNGObject.Create;
        try
          PNG.LoadFromFile(folder + l[i]);
          w := PNG.Width;
          h := PNG.Height;

          pdfimage := TPdfImage.Create(obPDF, PNG, true);
          obPage := obPDF.AddPage;
          obPage.PageWidth := w;
          obPage.PageHeight := h;
          sname := GetImgName(i) + '.png';
          obPDF.AddXObject(sname, pdfimage);
          obPDF.Canvas.DrawXObject(0, 0, w, h, sname);
        finally
          PNG.Free;
        end;
      end
      else
      begin
        JPG := TJPEGImage.Create;
        try
          if JPG_ReadWebp(JPG, folder + l[i]) then
          begin
            JPG.CompressionQuality := optjpegcompression;
            JPG.SaveToStream(m);
          end
          else
            m.LoadFromFile(folder + l[i]);

          w := JPG.Width;
          h := JPG.Height;
        finally
          JPG.Free;
        end;

        pdfimage := TPdfImage.CreateJpegDirect(obPDF, m);
        obPage := obPDF.AddPage;
        obPage.PageWidth := w;
        obPage.PageHeight := h;
        sname := GetImgName(i) + '.jpg';
        obPDF.AddXObject(sname, pdfimage);
        obPDF.Canvas.DrawXObject(0, 0, w, h, sname);
        m.Size := 0;
        m.Position := 0;
      end;
    end;

  m.Free;

  obPDF.SaveToFile(fname);
  obPDF.Free;
end;

procedure MakePDFEx(const folder: string; const l: TStringList; const fname: string);
var
  obPDF: TPdfDocument;
  obPage: TPdfPage;
  pdfimage: TPdfImage;
  JPG: TJPEGImage;
  i: integer;
  maxw, maxh, w, h: integer;
  sname: string;
  m: TMemoryStream;
  PNG: TPNGObject;
begin
  if l.Count = 0 then
    Exit;

  maxw := 0;
  maxh := 0;

  for i := 0 to l.Count - 1 do
  begin
    if RightStr(UpperCase(l[i]), 4) = '.PNG' then
    begin
      PNG := TPNGObject.Create;
      try
        PNG.LoadFromFile(folder + l[i]);
        w := PNG.Width;
        h := PNG.Height;
      finally
        PNG.Free;
      end;
    end
    else
    begin
      JPG := TJPEGImage.Create;
      try
        JPG_ReadWebp(JPG, folder + l[i]);
        w := JPG.Width;
        h := JPG.Height;
      finally
        JPG.Free;
      end;
    end;
    if optautosplitimages and (w > h) then
      w := w div 2;
    if w > maxw then
      maxw := w;
    if h > maxh then
      maxh := h;
  end;

  obPDF := TPdfDocument.Create(false, 0, false);
  obPDF.GeneratePDF15File := true;
  obPDF.DefaultPaperSize := psUserDefined;
  obPDF.DefaultPageWidth := maxw;
  obPDF.DefaultPageHeight := maxh;
  obPDF.DefaultPageLandscape := false;
  obPDF.CompressionMethod := cmNone;
  obPDF.ForceJPEGCompression := 0;
  obPDF.Info.Creator := 'PDFCreator';

  m := TMemoryStream.Create;
  for i := 0 to l.Count - 1 do
    if FileExists(folder + l[i]) then
    begin
      if RightStr(UpperCase(l[i]), 4) = '.PNG' then
      begin
        PNG := TPNGObject.Create;
        try
          PNG.LoadFromFile(folder + l[i]);
          w := PNG.Width;
          h := PNG.Height;

          pdfimage := TPdfImage.Create(obPDF, PNG, true);
          obPage := obPDF.AddPage;
          obPage.PageWidth := maxw;
          obPage.PageHeight := maxh;
          sname := GetImgName(i) + '.png';
          obPDF.AddXObject(sname, pdfimage);

          if optautosplitimages and (w > h) then
          begin
            obPDF.Canvas.DrawXObject(0, 0, 2 * maxw, maxh, sname);
            obPage := obPDF.AddPage;
            obPage.PageWidth := maxw;
            obPage.PageHeight := maxh;
            obPDF.Canvas.DrawXObject(-maxw, 0, 2 * maxw, maxh, sname);
          end
          else
          begin
            if w > h then
            begin
              obPage.PageWidth := 2 * maxw;
              obPDF.Canvas.DrawXObject(0, 0, 2 * maxw, maxh, sname);
            end
            else
              obPDF.Canvas.DrawXObject(0, 0, maxw, maxh, sname);
          end;
        finally
          PNG.Free;
        end;
      end
      else
      begin
        JPG := TJPEGImage.Create;
        try
          if JPG_ReadWebp(JPG, folder + l[i]) then
          begin
            JPG.CompressionQuality := optjpegcompression;
            JPG.SaveToStream(m);
          end
          else
            m.LoadFromFile(folder + l[i]);

          w := JPG.Width;
          h := JPG.Height;
        finally
          JPG.Free;
        end;

        pdfimage := TPdfImage.CreateJpegDirect(obPDF, m);
        obPage := obPDF.AddPage;
        obPage.PageWidth := maxw;
        obPage.PageHeight := maxh;
        sname := GetImgName(i) + '.jpg';
        obPDF.AddXObject(sname, pdfimage);

        if optautosplitimages and (w > h) then
        begin
          obPDF.Canvas.DrawXObject(0, 0, 2 * maxw, maxh, sname);
          obPage := obPDF.AddPage;
          obPage.PageWidth := maxw;
          obPage.PageHeight := maxh;
          obPDF.Canvas.DrawXObject(-maxw, 0, 2 * maxw, maxh, sname);
        end
        else
          obPDF.Canvas.DrawXObject(0, 0, maxw, maxh, sname);
        m.Size := 0;
        m.Position := 0;
      end;
    end;

  m.Free;

  obPDF.SaveToFile(fname);
  obPDF.Free;
end;

end.
