unit MainForm;

interface

uses
    Vcl.Forms
  ;

type
  TFormMain = class(TForm)
    procedure FormShow(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
    System.Classes
  , Data.DB
  , Datasnap.DBClient
  , MidasLib
  , System.SysUtils
  ;

procedure TFormMain.FormShow(Sender: TObject);
var
  DataSet: TDataSet;
begin
  DataSet := TClientDataSet.Create(nil);
  DataSet.FieldDefs.Add('FieldNum', ftInteger);
  DataSet.FieldDefs.Add('FieldStr', ftString, 3);
  DataSet.FieldDefs.Add('FieldDec', ftFloat);
  DataSet.FieldDefs.Add('FieldDate', ftDate);
  DataSet.FieldDefs.Add('FieldDateTime', ftDateTime);
  (DataSet as TClientDataSet).CreateDataSet;

  DataSet.Append;
//  DataSet.FieldByName('FieldNum').AsInteger := 111;
  DataSet.FieldByName('FieldNum').Clear;
//  DataSet.FieldByName('FieldStr').AsString := 'AAA';
  DataSet.FieldByName('FieldStr').Clear;
//  DataSet.FieldByName('FieldDec').AsFloat := 1.01;
  DataSet.FieldByName('FieldDec').Clear;
//  DataSet.FieldByName('FieldDate').AsDateTime := Date;
  DataSet.FieldByName('FieldDate').Clear;
//  DataSet.FieldByName('FieldDateTime').AsDateTime := Now;
  DataSet.FieldByName('FieldDateTime').Clear;
  DataSet.Post;

  DataSet.Append;
  DataSet.FieldByName('FieldNum').AsInteger := 222;
  DataSet.FieldByName('FieldStr').AsString := 'BBB';
  DataSet.FieldByName('FieldDec').AsFloat := 2.02;
  DataSet.FieldByName('FieldDate').AsDateTime := IncMonth(Date, 1);
  DataSet.FieldByName('FieldDateTime').AsDateTime := IncMonth(Now);
  DataSet.Post;

  DataSet.Free;
end;

end.
