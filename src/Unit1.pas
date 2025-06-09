unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, AdvSmoothImageListBox, jpeg, pngimage, webpimage,
  ExtDlgs, ShellAPI, StdCtrls, ComCtrls, Menus, Clipbrd;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    NewSpeedButton: TSpeedButton;
    OpenImgSpeedButton: TSpeedButton;
    ImportPDFSpeedButton: TSpeedButton;
    Panel2: TPanel;
    ImageListBox1: TAdvSmoothImageListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OpenDialog1: TOpenDialog;
    SaveCBZSpeedButton: TSpeedButton;
    SavePDFSpeedButton: TSpeedButton;
    SavePDFDialog1: TSaveDialog;
    ProgressPanel: TPanel;
    Label6: TLabel;
    ProgressBar1: TProgressBar;
    OpenDialog2: TOpenDialog;
    ReverseSpeedButton: TSpeedButton;
    Panel5: TPanel;
    BackPaintBox: TPaintBox;
    Panel6: TPanel;
    Panel7: TPanel;
    FrontPaintBox: TPaintBox;
    Panel8: TPanel;
    FrontCheckBox: TCheckBox;
    BackCheckBox: TCheckBox;
    Panel9: TPanel;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel10: TPanel;
    SplitCheckBox: TCheckBox;
    SaveCBZDialog1: TSaveDialog;
    ListPopupMenu: TPopupMenu;
    DeleteItem1: TMenuItem;
    N1: TMenuItem;
    Rotate1801: TMenuItem;
    Rotate90Clockwise1: TMenuItem;
    Rotate90Counterclockwise1: TMenuItem;
    N2: TMenuItem;
    PastePNG2: TMenuItem;
    PasteJPG2: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    OpenImages1: TMenuItem;
    ImportPDF1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    SaveCBZ1: TMenuItem;
    SavePDF1: TMenuItem;
    N5: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    PastePNG1: TMenuItem;
    N6: TMenuItem;
    ReverseOrder1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Copy2: TMenuItem;
    N7: TMenuItem;
    PasteJPG1: TMenuItem;
    N8: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenImgClick(Sender: TObject);
    procedure ImageListBox1ItemHint(Sender: TObject; itemindex: Integer;
      var hint: String);
    procedure ImageListBox1ItemSelect(Sender: TObject; itemindex: Integer);
    procedure NewClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ImportPDFClick(Sender: TObject);
    procedure SaveCBZClick(Sender: TObject);
    procedure SavePDFClick(Sender: TObject);
    procedure ReverseClick(Sender: TObject);
    procedure FrontPaintBoxPaint(Sender: TObject);
    procedure FrontPaintBoxDblClick(Sender: TObject);
    procedure BackPaintBoxPaint(Sender: TObject);
    procedure BackPaintBoxDblClick(Sender: TObject);
    procedure SplitCheckBoxClick(Sender: TObject);
    procedure ImageListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ImageListBox1DragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure ImageListBox1ItemStartDrag(Sender: TObject;
      ItemIndex: Integer; var AllowDrag: Boolean);
    procedure CheckEditMenuEnable(Sender: TObject);
    procedure DeleteItem1Click(Sender: TObject);
    procedure Rotate1801Click(Sender: TObject);
    procedure Rotate90Clockwise1Click(Sender: TObject);
    procedure Rotate90Counterclockwise1Click(Sender: TObject);
    procedure PastePNGClick(Sender: TObject);
    procedure PasteJPGClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
  private
    { Private declarations }
    frontpage, backpage: string;
    dragitem: integer;
    procedure PageDraw(const pb: TPaintBox; const spage: string);
    function AddImageToList(const lb: TAdvSmoothImageListBox; const imgpath: string): Boolean;
    procedure ShowSelectedImgInfo(itemindex: integer);
    procedure LoadTextFromImages;
    procedure AdjustLayout;
    procedure ProgressStart(const cnt: integer);
    procedure ProgressDone;
    procedure Progress(const x: integer);
    procedure OnPDFProgress(const pct: Single);
    procedure AddFilesToList(const aFiles: TStrings);
    function GetFrontPage: string;
    function GetBackPage: string;
    function GetImageFilenames: TStringList;
    procedure RotateSelectedImage(const degrees: integer);
    function PasteNewImageAs(const ext: string): Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  utils, img_utils, options, pdfexport, webp, tmpfiles, pdf2jpg, wzipfile, 
  img_names, rarfile;

{$R *.dfm}

resourcestring
    s_iniFile = 'PDFCreator.ini';

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  DoubleBuffered := True;
  for i := 0 to ComponentCount - 1 do
    if Components[i].InheritsFrom(TWinControl) then
      (Components[i] as TWinControl).DoubleBuffered := True;

  LoadDefaults(ExtractFilePath(ParamStr(0)) + s_iniFile);
  SplitCheckBox.Checked := optautosplitimages;
  if optjpegcompression < 75 then
    optjpegcompression := 75
  else if optjpegcompression > 100 then
    optjpegcompression := 100;

  AdjustLayout;
  ShowSelectedImgInfo(-1);
  frontpage := '';
  backpage := '';
  dragitem := -1;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SaveDefaults(ExtractFilePath(ParamStr(0)) + s_iniFile);
end;

procedure TForm1.LoadTextFromImages;
var
  i: integer;
begin
  for I := 0 to ImageListBox1.Items.Count - 1 do
  begin
    with ImageListBox1.Items[I] do
    begin
      Caption.Text := ExtractFileName(Location);
      Caption.Location := cpCenterCenter;
    end;
  end;
end;

function TForm1.AddImageToList(const lb: TAdvSmoothImageListBox; const imgpath: string): Boolean;
var
  item: TAdvSmoothImageListBoxItem;
  jpath: string;
  wp: TWEBPImage;
begin
  if Trim(imgpath) = '' then
  begin
    Result := False;
    Exit;
  end;

  Result := True;

  item := lb.Items.Add;

  try
    if IsWebpImage(imgpath) then
    begin
      jpath := I_NewTempFile(imgpath + '.jpg');
      wp := TWEBPImage.Create;
      try
        wp.LoadFromFile(imgpath);
        wp.CompressionQuality := 100;
        wp.SaveToFile(jpath);
      finally
        wp.Free;
      end;
      item.Image.LoadFromFile(jpath);
      item.FileSize := fsize(jpath);
      item.FileDate := GetFileCreationTime(jpath);
      item.FileName := ExpandFileName(jpath);
      item.Location := ExpandFileName(jpath);
    end
    else
    begin
      item.Image.LoadFromFile(imgpath);
      item.FileSize := fsize(imgpath);
      item.FileDate := GetFileCreationTime(imgpath);
      item.FileName := ExpandFileName(imgpath);
      item.Location := ExpandFileName(imgpath);
    end;
    if optSetImageCaption then
    begin
      if optNoExt then
        item.Caption.Text := ExtractFileName(ChangeFileExt(imgpath, ''))
      else
        item.Caption.Text := ExtractFileName(imgpath);

      item.Caption.Location := cpCenterCenter;
    end;
    if optSetImageHint then
    begin
      if optNoExt then
        item.Hint := ExtractFileName(ChangeFileExt(imgpath, ''))
      else
        item.Hint := ExtractFileName(imgpath);
    end;

  except
    item.Image.Assign(nil);
    Result := False;
  end;
end;

procedure TForm1.AddFilesToList(const aFiles: TStrings);
var
  i, j: integer;
  zip: TZipFile;
  rar: TRarFile;
  ext, ext2: string;
  Files: TStringList;
  afile, file1, file2: string;
  l1, l2: TStringList;
  cnt: integer;
begin
  cnt := 0;
  Screen.Cursor := crHourGlass;
  Files := TStringList.Create;
  try
    for i := 0 to aFiles.Count - 1 do
    begin
      afile := aFiles.Strings[i];
      ext := LowerCase(ExtractFileExt(afile));
      if (ext = '.zip') or (ext = '.cbz') then
      begin
        l1 := TStringList.Create;
        l2 := TStringList.Create;
        zip := TZipFile.Create(afile);
        for j := 0 to zip.FileCount - 1 do
        begin
          file1 := zip.Files[j];
          ext2 := LowerCase(ExtractFileExt(file1));
          if (ext2 = '.png') or (ext2 = '.jpg') or (ext2 = '.jpeg') or (ext2 = '.webp') or (ext2 = '.bmp') then
          begin
            l1.Add(file1);
            file2 := I_NewTempFile('arc' + IntToStr4(cnt) + ext2);
            if FileExists(file2) then
              DeleteFile(file2);
            Inc(cnt);
            Files.Add(file2);
            l2.Add(file2);
          end;
        end;
        l1.CustomSort(sort_logical);
        zip.ExtractFiles(l1, l2);
        zip.Free;
        l1.Free;
        l2.Free;
      end
      else if (ext = '.rar') or (ext = '.cbr') then
      begin
        l1 := TStringList.Create;
        l2 := TStringList.Create;
        rar := TRarFile.Create(afile);
        for j := 0 to rar.FileCount - 1 do
        begin
          file1 := rar.Files[j];
          ext2 := LowerCase(ExtractFileExt(file1));
          if (ext2 = '.png') or (ext2 = '.jpg') or (ext2 = '.jpeg') or (ext2 = '.webp') or (ext2 = '.bmp') then
          begin
            l1.Add(file1);
            file2 := I_NewTempFile('arc' + IntToStr4(cnt) + ext2);
            if FileExists(file2) then
              DeleteFile(file2);
            Inc(cnt);
            Files.Add(file2);
            l2.Add(file2);
          end;
        end;
        l1.CustomSort(sort_logical);
        rar.ExtractFiles(l1, l2);
        rar.Free;
        l1.Free;
        l2.Free;
      end
      else
        Files.Add(afile);
    end;
    ImageListBox1.Items.BeginUpdate;
    ImageListBox1.Items.Clear;
    ProgressStart(Files.Count);
    for i := 0 to Files.Count - 1 do
    begin
      AddImageToList(ImageListBox1, Files[i]);
      Progress(i + 1);
    end;
    ProgressDone;
    LoadTextFromImages;
    ImageListBox1.Items.EndUpdate;

    if ImageListBox1.Items.Count > 0 then
    begin
      ImageListBox1.SelectedItemIndex := 0;
      ShowSelectedImgInfo(0);
    end
    else
      ShowSelectedImgInfo(-1);
  finally
    Screen.Cursor := crDefault;
    Files.Free;
  end;

  ImageListBox1.SetFocus;
end;

procedure TForm1.OpenImgClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    AddFilesToList(OpenDialog1.Files);
end;

procedure TForm1.ImageListBox1ItemHint(Sender: TObject; itemindex: Integer;
  var hint: String);
begin
  hint := ImageListBox1.Items[itemindex].Caption.Text;
end;

procedure TForm1.ShowSelectedImgInfo(itemindex: integer);
type
  TFileInfo = record
    Icon: hIcon;
    Image: Integer;
    DisplayName: String;
    TypeName: String;
    Size: Integer;
    SizeDescription: String;
    DateTime: TDateTime;
    AttrArchive: Boolean;
    AttrReadOnly: Boolean;
    AttrSystem: Boolean;
    AttrHidden: Boolean;
    AttrVolume: Boolean;
    AttrDirectory: Boolean;
  end;

  function scGetSizeDescription(const IntSize: Int64) : String;
  begin
    if IntSize < 1024 then
      Result := IntToStr(IntSize) + ' bytes'
    else
    begin
      if IntSize < (1024 * 1024) then
        Result := FormatFloat('####0.##', IntSize / 1024) + ' Kb'
      else if IntSize < (1024 * 1024 * 1024) then
        Result := FormatFloat('####0.##', IntSize / 1024 / 1024) + ' Mb'
      else
        Result := FormatFloat('####0.##', IntSize / 1024 / 1024 / 1024) + ' Gb';
    end;
  end;

  procedure scGetFileInfo(StrPath : String; var Info : TFileInfo);
  var
    SHFileInfo: TSHFileInfo;
    SearchRec: TSearchRec;
  begin
    if Trim(StrPath) = '' then
      Exit;

    ShGetFileInfo(PChar(StrPath), 0, SHFileInfo, SizeOf (TSHFileInfo),
      SHGFI_TYPENAME or SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX or SHGFI_ICON);

    with Info do
    begin
      Image := SHFileInfo.iIcon;
      DisplayName := SHFileInfo.szDisplayName;
      TypeName := SHFileInfo.szTypeName;
    end;

    FindFirst(StrPath, 0, SearchRec);
    with Info do
    begin
      try
        DateTime := FileDateToDateTime(SearchRec.Time);
      except
        DateTime := Now();
      end;

      AttrReadOnly := ((SearchRec.Attr and faReadOnly) > 0);
      AttrSystem := ((SearchRec.Attr and faSysFile) > 0);
      AttrHidden := ((SearchRec.Attr and faHidden) > 0);
      AttrArchive := ((SearchRec.Attr and faArchive) > 0);
      AttrVolume := ((SearchRec.Attr and faVolumeID) > 0);
      AttrDirectory := ((SearchRec.Attr and faDirectory) > 0);

      Size := SearchRec.Size;

      SizeDescription := scGetSizeDescription(Size);
    end;
  end;

var
  a, b, c, d, e: String;
  info: TFileInfo;
begin
  if itemindex < 0 then
  begin
    Label1.Caption := '-';
    Label2.Caption := '-';
    Label3.Caption := '-';
    Label4.Caption := '-';
    Label5.Caption := '-';
    Exit;
  end;

   scGetFileInfo(ImageListBox1.Items[itemindex].Location, info);
   //Image Number
   a := 'Image #' + IntToStr(itemindex + 1);
   //Image Name
   b := ExtractFileName(ImageListBox1.Items[itemindex].Location);
   //Image res
   c := IntToStr(ImageListBox1.Items[itemindex].GetOriginalImageWidth) + ' x ' + IntToStr(ImageListBox1.Items[itemindex].GetOriginalImageHeight);
   //Image File Date
   d := FormatDateTime('dd/mm/yy',info.DateTime);
   //Image File Size
   e := scGetSizeDescription(info.Size);

   Label1.Caption := a;
   Label2.Caption := 'Image Name : ' + b;
   Label3.Caption := 'Image Resolution : ' + c;
   Label4.Caption := 'Image File Date : ' + d;
   Label5.Caption := 'Image File Size : ' + e;
end;

procedure TForm1.ImageListBox1ItemSelect(Sender: TObject;
  itemindex: Integer);
begin
  ShowSelectedImgInfo(Itemindex);
end;

procedure TForm1.NewClick(Sender: TObject);
begin
  ImageListBox1.Items.BeginUpdate;
  ImageListBox1.Items.Clear;
  ImageListBox1.Items.EndUpdate;
  ShowSelectedImgInfo(-1);
  frontpage := '';
  backpage := '';
  FrontPaintBox.Invalidate;
  BackPaintBox.Invalidate;
  dragitem := -1;
end;

procedure TForm1.AdjustLayout;
var
  n: integer;
begin
  if WindowState <> wsMinimized then
  begin
    n := Width div (ImageListBox1.ItemAppearance.ItemWidth + 2 * ImageListBox1.ItemAppearance.ItemHorizontalSpacing);
    if n > 0 then
      ImageListBox1.Columns := n
    else
      ImageListBox1.Columns := 1;
//    ProgressPanel.Left := (ImageListBox1.Width - ProgressPanel.Width) div 2;
//    ProgressPanel.Top := (ImageListBox1.Height - ProgressPanel.Height) div 2;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  AdjustLayout;
end;

function TForm1.GetImageFilenames: TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  if GetFrontPage <> '' then
    Result.Add(GetFrontPage);
  for i := 0 to ImageListBox1.Items.Count - 1 do
    Result.Add(ImageListBox1.Items[i].Location);
  if GetBackPage <> '' then
    Result.Add(GetBackPage);
end;

const
  PDFPROGRESSMAX = 200;

procedure TForm1.ImportPDFClick(Sender: TObject);
var
  i: integer;
  pdf: TPDF2Jpeg;
  afile: string;
begin
  if OpenDialog2.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      ImageListBox1.Items.BeginUpdate;
      ImageListBox1.Items.Clear;
      ProgressStart(PDFPROGRESSMAX);
      pdf := TPDF2Jpeg.Create;
      pdf.OnProgress := OnPDFProgress;
      pdf.AppendPDFFromFile(OpenDialog2.FileName);
      ProgressStart(pdf.Count);
      if pdf.reverseorder then
      begin
        for i := pdf.Count - 1 downto 0 do
        begin
          afile := I_NewTempFile(IntToStr4(i + 1) + '.jpg');
          pdf.SaveImageTo(i, afile);
          AddImageToList(ImageListBox1, afile);
          Progress(i + 1);
        end;
      end
      else
      begin
        for i := 0 to pdf.Count - 1 do
        begin
          afile := I_NewTempFile(IntToStr4(i + 1) + '.jpg');
          pdf.SaveImageTo(i, afile);
          AddImageToList(ImageListBox1, afile);
          Progress(i + 1);
        end;
      end;
      ProgressDone;
      pdf.Free;
      LoadTextFromImages;
      ImageListBox1.Items.EndUpdate;

      if ImageListBox1.Items.Count > 0 then
      begin
        ImageListBox1.SelectedItemIndex := 0;
        ShowSelectedImgInfo(0);
      end
      else
        ShowSelectedImgInfo(-1);
    finally
      Screen.Cursor := crDefault;
    end;

    ImageListBox1.SetFocus;
  end;
end;

procedure TForm1.SaveCBZClick(Sender: TObject);
var
  pk3: TWZipFile;
  l: TStringList;
  i: integer;
begin
  if SaveCBZDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;
    l := GetImageFilenames;
    pk3 := TWZipFile.Create;
    try
      for i := 0 to l.Count - 1 do
        AddFileToZip(pk3, l.Strings[i], GetImgName(i) + ExtractFileExt(l.Strings[i]));
      backupfile(SaveCBZDialog1.FileName);
      pk3.SaveToFile(SaveCBZDialog1.FileName);
    finally
      l.Free;
      pk3.Free;
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.SavePDFClick(Sender: TObject);
var
  l: TStringList;
begin
  if SavePDFDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;
    l := GetImageFilenames;
    if l.Count > 0 then
    try
      backupfile(SavePDFDialog1.FileName);
      MakePDFEx('', l, SavePDFDialog1.FileName);
    finally
      l.Free;
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.ReverseClick(Sender: TObject);
var
  l: TStringList;
  i: integer;
begin
  l := TStringList.Create;
  for i := ImageListBox1.Items.Count - 1 downto 0 do
    l.Add(ImageListBox1.Items[i].Location);
  AddFilesToList(l);
  l.Free;
end;

procedure TForm1.ProgressStart(const cnt: integer);
begin
  ProgressBar1.Min := 0;
  ProgressBar1.Max := cnt;
  if cnt > 1 then
  begin
//    AdjustLayout;
    ProgressPanel.Visible := True;
    ProgressBar1.Position := 0;
    ProgressPanel.Repaint;
  end;
end;

procedure TForm1.ProgressDone;
begin
  ProgressPanel.Visible := False;
end;

procedure TForm1.Progress(const x: integer);
begin
  if ProgressPanel.Visible then
  begin
    ProgressBar1.Position := x;
    ProgressPanel.Repaint;
  end;
end;

procedure TForm1.OnPDFProgress(const pct: Single);
begin
  Progress(Round(pct * PDFPROGRESSMAX));
end;

procedure TForm1.PageDraw(const pb: TPaintBox; const spage: string);
var
  r: TRect;
  img: TImage;
begin
  pb.Canvas.Pen.Style := psSolid;
  pb.Canvas.Pen.Color := RGB(255, 255, 255);
  pb.Canvas.Brush.Style := bsSolid;
  pb.Canvas.Brush.Color := RGB(255, 255, 255);
  pb.Canvas.Rectangle(0, 0, pb.Width, pb.Height);

  if spage = '' then
    Exit;

  if not FileExists(spage) then
    Exit;

  img := TImage.Create(nil);
  img.Picture.LoadFromFile(spage);
  r := GetStretchRect(Rect(0, 0, pb.Width, pb.Height), img.Picture.Graphic.Width, img.Picture.Graphic.Height);
  pb.Canvas.StretchDraw(r, img.Picture.Graphic);
  img.Free;
end;

procedure TForm1.FrontPaintBoxPaint(Sender: TObject);
begin
  PageDraw(FrontPaintBox, frontpage);
end;

procedure TForm1.FrontPaintBoxDblClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    frontpage := OpenPictureDialog1.FileName;
    FrontPaintBox.Invalidate;
  end;
end;

procedure TForm1.BackPaintBoxPaint(Sender: TObject);
begin
  PageDraw(BackPaintBox, backpage);
end;

procedure TForm1.BackPaintBoxDblClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    backpage := OpenPictureDialog1.FileName;
    BackPaintBox.Invalidate;
  end;
end;

procedure TForm1.SplitCheckBoxClick(Sender: TObject);
begin
  optautosplitimages := SplitCheckBox.Checked;
end;

function TForm1.GetFrontPage: string;
begin
  Result := '';
  if FrontCheckBox.Checked then
    if FileExists(frontpage) then
      Result := frontpage;
end;

function TForm1.GetBackPage: string;
begin
  Result := '';
  if BackCheckBox.Checked then
    if FileExists(backpage) then
      Result := backpage;
end;

procedure TForm1.ImageListBox1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = ImageListBox1;
  if Accept then
    Accept := ImageListBox1.FindItemOnXY(X, Y) >= 0;
end;

procedure TForm1.ImageListBox1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  dst: integer;
  src: integer;
  l: TStringList;
  i: integer;
  sitem: string;
begin
  dst := ImageListBox1.FindItemOnXY(X, Y);

  src := dragitem;
  dragitem := -1;

  if dst < 0 then
    Exit;

  if src < 0 then
    Exit;

  if src = dst then
    Exit;

  l := TStringList.Create;
  for i := 0 to ImageListBox1.Items.Count - 1 do
    l.Add(ImageListBox1.Items[i].Location);
  sitem := l.Strings[src];
  if dst < src then
  begin
    l.Insert(dst, sitem);
    l.Delete(src + 1)
  end
  else
  begin
    l.Insert(dst + 1, sitem);
    l.Delete(src);
  end;
  AddFilesToList(l);
  l.Free;
end;

procedure TForm1.ImageListBox1ItemStartDrag(Sender: TObject;
  ItemIndex: Integer; var AllowDrag: Boolean);
begin
  dragitem := ItemIndex;
  AllowDrag := dragitem >= 0;
end;

procedure TForm1.CheckEditMenuEnable(Sender: TObject);
var
  sel: integer;
  pp: TPoint;
begin
  pp := ImageListBox1.ScreenToClient(ListPopupMenu.PopupPoint);
  sel := ImageListBox1.FindItemOnXY(pp.X, pp.Y);
  if sel >= 0 then
  begin
    ImageListBox1.SelectedItemIndex := sel;
    ShowSelectedImgInfo(sel);
  end;
  DeleteItem1.Enabled := ImageListBox1.SelectedItemIndex >= 0;
  Copy1.Enabled := ImageListBox1.SelectedItemIndex >= 0;
  Copy2.Enabled := ImageListBox1.SelectedItemIndex >= 0;
  Rotate1801.Enabled := ImageListBox1.SelectedItemIndex >= 0;
  Rotate90Clockwise1.Enabled := ImageListBox1.SelectedItemIndex >= 0;
  Rotate90Counterclockwise1.Enabled := ImageListBox1.SelectedItemIndex >= 0;
  PastePNG1.Enabled := IsClipboardFormatAvailable(CF_BITMAP);
  PastePNG2.Enabled := PastePNG1.Enabled;
  PasteJPG1.Enabled := IsClipboardFormatAvailable(CF_BITMAP);
  PasteJPG2.Enabled := PasteJPG1.Enabled;
end;

procedure TForm1.DeleteItem1Click(Sender: TObject);
var
  sel: integer;
  l: TStringList;
  i: integer;
begin
  sel := ImageListBox1.SelectedItemIndex;
  if sel < 0 then
    Exit;

  l := TStringList.Create;
  try
    for i := 0 to ImageListBox1.Items.Count - 1 do
      if i <> sel then
        l.Add(ImageListBox1.Items[i].Location);
    AddFilesToList(l);
  finally
    l.Free;
  end;
end;

procedure TForm1.RotateSelectedImage(const degrees: integer);
var
  sel: integer;
  l: TStringList;
  i: integer;
  fin, fout: string;
  ret: boolean;
begin
  sel := ImageListBox1.SelectedItemIndex;
  if sel < 0 then
    Exit;

  if degrees <> 90 then
    if degrees <> 180 then
      if degrees <> 270 then
        Exit;

  ret := False;
  l := TStringList.Create;
  try
    for i := 0 to ImageListBox1.Items.Count - 1 do
      if i <> sel then
        l.Add(ImageListBox1.Items[i].Location)
      else
      begin
        fin := ImageListBox1.Items[i].Location;
        fout := I_NewGlobalTempFile(ChangeFileExt(ExtractFileName(fin), '.png'));
        if degrees = 90 then
          ret := RotateImageFile90DegreesClockwise(fin, fout)
        else if degrees = 270 then
          ret := RotateImageFile90DegreesCounterClockwise(fin, fout)
        else
          ret := RotateImageFile180Degrees(fin, fout);
        if ret then
          l.Add(fout)
        else
          l.Add(ImageListBox1.Items[i].Location);
      end;
    if ret then
      AddFilesToList(l);
  finally
    l.Free;
  end;
  ImageListBox1.SelectedItemIndex := sel;
  ShowSelectedImgInfo(ImageListBox1.SelectedItemIndex)
end;

procedure TForm1.Rotate1801Click(Sender: TObject);
begin
  RotateSelectedImage(180);
end;

procedure TForm1.Rotate90Clockwise1Click(Sender: TObject);
begin
  RotateSelectedImage(90);
end;

procedure TForm1.Rotate90Counterclockwise1Click(Sender: TObject);
begin
  RotateSelectedImage(270);
end;

function TForm1.PasteNewImageAs(const ext: string): Boolean;
var
  sel, idx: integer;
  l: TStringList;
  i: integer;
  bm: TBitmap;
  imgfname: string;
begin
  Result := False;

  if not IsClipboardFormatAvailable(CF_BITMAP) then
    Exit;

  imgfname := '';
  bm := TBitmap.Create;
  try
    bm.Assign(Clipboard);
    if (bm.Width > 0) and (bm.Height > 0) then
    begin
      imgfname := I_NewGlobalTempFile('paste' + ext);
      SaveBitmapToFile(bm, imgfname);
    end;
  finally
    bm.Free;
  end;

  if imgfname = '' then
    Exit;

  if not FileExists(imgfname) then
    Exit;

  sel := ImageListBox1.SelectedItemIndex;
  idx := sel;

  l := TStringList.Create;
  try
    for i := 0 to ImageListBox1.Items.Count - 1 do
      l.Add(ImageListBox1.Items[i].Location);
    if sel < 0 then
      idx := l.Add(imgfname)
    else
      l.Insert(sel, imgfname);
    AddFilesToList(l);
  finally
    l.Free;
  end;
  if idx >= 0 then
    ImageListBox1.SelectedItemIndex := idx;
  ShowSelectedImgInfo(ImageListBox1.SelectedItemIndex);

  Result := True;
end;

procedure TForm1.PastePNGClick(Sender: TObject);
begin
  PasteNewImageAs('.png');
end;

procedure TForm1.PasteJPGClick(Sender: TObject);
begin
  PasteNewImageAs('.jpg');
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
    MessageBox(
      Handle,
      PChar(Format('%s'#13#10'Version %s'#13#10#13#10'A tool to create comic PDF files.'#13#10'© 2025, jvalavanis@gmail.com',
        [DEF_APPNAME, I_VersionBuilt])),
      PChar(DEF_APPNAME),
      MB_OK or MB_ICONINFORMATION or MB_APPLMODAL);
end;

end.
