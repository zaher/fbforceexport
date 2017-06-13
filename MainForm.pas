unit MainForm;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, mncConnections, mncDB, LCLType,
  mncCSV, mncFirebird, mncFBClient;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DatabaseEdit: TEdit;
    Label1: TLabel;
    OpenDialog: TOpenDialog;
    PathEdit: TEdit;
    ResultMemo: TMemo;
    TablesList: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    Connection: TmncFBConnection;
    Session: TmncFBSession;
    csvCnn: TmncCSVConnection;
    csvSes: TmncCSVSession;
    Cancel: Boolean;
    Count: Int64; //here cuz safe from error/exceptions
  public
    procedure DBOpen;
    procedure DBClose;
    procedure ExportAll;
    procedure ExportTable(TableName: string);
  end;

var
  Form1: TForm1;

implementation

uses
  mnStreams;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  TablesList.Lines.LoadFromFile(Application.Location + 'tables.txt');
  if FileExists(Application.Location + 'fb/fbembed.dll') then
    InitEmbedMode(Application.Location + 'fb/fbembed.dll');
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Cancel := True;
end;

procedure TForm1.DBOpen;
begin
  csvCnn := TmncCSVConnection.Create;
  Connection := TmncFBConnection.Create;
  Connection.Resource := DatabaseEdit.Text;
  //Connection.CharacterSet := 'WIN1252';
  //Connection.CharacterSet := 'UTF8';
  Connection.CharacterSet := 'NONE';
  Connection.UserName := 'sysdba';
  Connection.Password := 'masterkey';
  Connection.ErrorHandles := [];
  Connection.Open;

  Session := Connection.CreateSession as TmncFBSession;
  //Session.Params.Add('read_only');
  Session.Params.Add('ignore_limbo');
  Session.Start;

  csvCnn.Connect;

  csvSes := TmncCSVSession.Create(csvCnn);
  csvSes.HeaderLine := hdrNormal;
  csvSes.DelimiterChar := ';';
  csvSes.EndOfLine := sWinEndOfLine;
  csvSes.Start;
end;

procedure TForm1.DBClose;
begin
  FreeAndNil(csvSes);
  FreeAndNil(csvCnn);
  FreeAndNil(Session);
  FreeAndNil(Connection);
end;

procedure TForm1.ExportAll;
var
  i: Integer;
  s: string;
//  CMD: TmncFBCommand;
begin
  Cancel := False;
  //DBOpen;
  try
//    CMD := Session.CreateCommand as TmncFBCommand;
    //CMD.SQL.Text := 'delete from "RDB$TRIGGERS"';
    //CMD.Execute;
{    CMD.SQL.Text := 'update rdb$triggers set rdb$trigger_inactive = 1';
    CMD.Execute;
    CMD.SQL.Text := 'delete from "RDB$INDICES" where RDB$SYSTEM_FLAG = 0';
    CMD.Execute;
    CMD.Close;
    exit;}

    for i := 0 to TablesList.Lines.Count - 1 do
    begin
      s:= trim(TablesList.Lines[i]);
      Count := 0;
      if s <> '' then
      begin
        DBOpen;

        try
          try
            ExportTable(s);
          except
            on E: Exception do
              ResultMemo.Lines.Add(E.Message);
          end;
        finally
          DBClose;
        end;

        ResultMemo.Lines.Add(s + ' is exported: ' + IntToStr(Count));
        ResultMemo.Lines.Add('---------------------');
        Application.ProcessMessages;
      end;
    end;
    ResultMemo.Lines.Add('Done.');
  finally
    //CMD.Free;
    //DBClose;
  end;
end;

procedure TForm1.ExportTable(TableName: string);
var
  CMD: TmncFBCommand;
  csvCMD: TmncCSVCommand;
  aStream: TFileStream;
  i: Integer;
begin
  Count := 0;
  aStream := TFileStream.Create(IncludeTrailingPathDelimiter(PathEdit.Text)+TableName+'.csv', fmCreate);
  try
    CMD := Session.CreateCommand as TmncFBCommand;
    CMD.SQL.Text := 'select * from "'+TableName+'"';
    CMD.SQL.Add('PLAN ("'+TableName+'" NATURAL)');

    CMD.Prepare;

    csvCMD := TmncCSVCommand.Create(csvSes, aStream, csvmWrite);
    try

      csvCMD.Columns.Clone(CMD.Columns, dtString);
      csvCMD.Prepare;
      CMD.Execute;
      while not CMD.Done do
      begin
        for i := 0 to CMD.Columns.Count -1 do
        begin
          try
            csvCMD.Params.Items[i].AsString := CMD.Fields.Items[i].AsString;
          except
            csvCMD.Params.Items[i].AsString := '(BLOB)';
          end;
        end;
        csvCMD.Execute;
        try
          CMD.Next;
        except
          //ResultMemo.Lines.Add('ERROR:');
          raise;
        end;
        Inc(Count);
        if (Count mod 1000) = 0 then
        begin
          Application.ProcessMessages;
          if Cancel then
            break;
        end;
      end;

    finally
      csvCMD.Session.Commit(true);
      csvCMD.Free;
      CMD.Free;
    end;
  finally
    FreeAndNil(aStream);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ExportAll;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    DatabaseEdit.Text := OpenDialog.FileName;
  end;
end;

end.

