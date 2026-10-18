unit frm_options;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TOptionsForm = class(TForm)
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    ImageCaptionCheckBox: TCheckBox;
    HideExtCheckBox: TCheckBox;
    ImageHintsCheckBox: TCheckBox;
    CBZRadioGroup: TRadioGroup;
    JPGTrackBar: TTrackBar;
    Label1: TLabel;
    AutoSplitCheckBox: TCheckBox;
    PDFSizeRadioGroup: TRadioGroup;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ShowOptionsForm: Boolean;

implementation

{$R *.dfm}

uses
  options;

function ShowOptionsForm: Boolean;
var
  f: TOptionsForm;
  _old_optSetImageCaption: Boolean;
  _old_optNoExt: Boolean;
  _old_optSetImageHint: Boolean;
  _old_optjpegcompression: integer;
  _old_optautosplitimages: Boolean;
  _old_optpdfexportsize: Integer;
  _old_optzipcompression: Integer;
begin
  Result := False;
  f := TOptionsForm.Create(nil);
  try

    _old_optSetImageCaption := optSetImageCaption;
    _old_optNoExt := optNoExt;
    _old_optSetImageHint := optSetImageHint;
    _old_optjpegcompression := optjpegcompression;
    _old_optautosplitimages := optautosplitimages;
    _old_optpdfexportsize := optpdfexportsize;
    _old_optzipcompression := optzipcompression;

    f.ImageCaptionCheckBox.Checked := optSetImageCaption;
    f.HideExtCheckBox.Checked := optNoExt;
    f.ImageHintsCheckBox.Checked := optSetImageHint;
    f.CBZRadioGroup.ItemIndex := optzipcompression;
    f.JPGTrackBar.Position := (optjpegcompression - 50) div 5;
    f.AutoSplitCheckBox.Checked := optautosplitimages;
    f.PDFSizeRadioGroup.ItemIndex := optpdfexportsize;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      optSetImageCaption := f.ImageCaptionCheckBox.Checked;
      optNoExt := f.HideExtCheckBox.Checked;
      optSetImageHint := f.ImageHintsCheckBox.Checked;
      optzipcompression := f.CBZRadioGroup.ItemIndex;
      optjpegcompression := 50 + f.JPGTrackBar.Position * 5;
      optautosplitimages := f.AutoSplitCheckBox.Checked;
      optpdfexportsize := f.PDFSizeRadioGroup.ItemIndex;

      Result :=
        (_old_optSetImageCaption <> optSetImageCaption) or
        (_old_optNoExt <> optNoExt) or
        (_old_optSetImageHint <> optSetImageHint) or
        (_old_optjpegcompression <> optjpegcompression) or
        (_old_optautosplitimages <> optautosplitimages) or
        (_old_optpdfexportsize <> optpdfexportsize) or
        (_old_optzipcompression <> optzipcompression);
    end;
  finally
    f.Free;
  end;

end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

end.
