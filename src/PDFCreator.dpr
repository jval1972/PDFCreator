program PDFCreator;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
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

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
