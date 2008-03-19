unit isolate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons;

type

  { TIsolateForm }

  TIsolateForm = class ( TForm )
    ArchiveButton: TButton;
    CatalogueButton: TButton;
    CancelButton: TBitBtn;
    ArchiveBox: TLabeledEdit;
    CatalogueBox: TLabeledEdit;
    OKButton: TBitBtn;
    OKCancelPanel: TPanel;
    MainPanel: TPanel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure ArchiveButtonClick ( Sender: TObject ) ;
    procedure CatalogueButtonClick ( Sender: TObject ) ;
    procedure OKButtonClick ( Sender: TObject ) ;
    procedure OnEditBoxExit ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end; 


implementation

uses darintf;

{ TIsolateForm }

procedure TIsolateForm.OnEditBoxExit ( Sender: TObject ) ;
var
  fn: String;
begin
  if TEdit(Sender).Text <> '' then
     begin
       TEdit(Sender).Text := TrimToBase(TEdit(Sender).Text);
       if (Sender = ArchiveBox) and (CatalogueBox.Text = '')
          then CatalogueBox.Text := ArchiveBox.Text + '_cat';
     end;
end;

procedure TIsolateForm.ArchiveButtonClick ( Sender: TObject ) ;
begin
  if FileExists(ArchiveBox.Text)
     then OpenDialog.InitialDir := ExtractFilePath( ArchiveBox.Text )
     else OpenDialog.InitialDir := SysUtils.GetEnvironmentVariable('HOME');
  if OpenDialog.Execute then
     begin
       ArchiveBox.Text := TrimToBase(OpenDialog.FileName);
       OnEditBoxExit(ArchiveBox);
     end;
end;

procedure TIsolateForm.CatalogueButtonClick ( Sender: TObject ) ;
begin
  if CatalogueBox.Text <> ''
     then SaveDialog.InitialDir := ExtractFilePath( CatalogueBox.Text )
     else SaveDialog.InitialDir := SysUtils.GetEnvironmentVariable('HOME');
  if SaveDialog.Execute
     then CatalogueBox.Text := TrimToBase(SaveDialog.FileName);
end;

procedure TIsolateForm.OKButtonClick ( Sender: TObject ) ;
var
  fn: String;
begin
  fn := ArchiveBox.Text + '.1.dar';
  if FileExists(fn)
     then ModalResult := mrOk
     else begin
      if MessageDlg('Error', 'Unable to find archive ''' + fn + '''',
                           mtError, mbOKCancel, 0) = mrOK
           then ArchiveBox.SetFocus
           else ModalResult := mrCancel;
     end;
end;

initialization
  {$I isolate.lrs}

end.

