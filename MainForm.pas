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
    PathEdit: TEdit;
    ResultMemo: TMemo;
    TablesList: TMemo;
    procedure Button1Click(Sender: TObject);
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
    procedure ExportIt;
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
  InitEmbedMode(Application.Location + 'fb/fbembed.dll');
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Cancel := True;
end;

procedure TForm1.ExportIt;
var
  CMD: TmncFBCommand;
  aConnection: TmncFBConnection;
  aSession: TmncFBSession;
  t: ansiString;
begin
  //DefaultSystemCodePage := CP_NONE;
  ResultMemo.Clear;
  aConnection := TmncFBConnection.Create;
  try
    aConnection.Resource := 'd:\temp\data.fdb';
    //Connection.CharacterSet := 'WIN1252';
    aConnection.CharacterSet := 'NONE';
    aConnection.UserName := 'sysdba';
    aConnection.Password := 'masterkey';
    aConnection.Open;
    aSession := aConnection.CreateSession as TmncFBSession;
    //Session.Params.Add('read_only');
    aSession.Params.Add('ignore_limbo');
    aSession.Start;
    CMD := aSession.CreateCommand as TmncFBCommand;
    CMD.SQL.Text := 'select * from "Accounts"';
    CMD.Execute;
    while not CMD.Done do
    begin
      t := #$D2#$C7#$E5#$D1; //CMD.Field['AccName'].AsString;
      ResultMemo.Lines.Add(UTF8Encode(t));
      CMD.Next;
      break;
    end;
    CMD.Free;
  finally
    aSession.Free;
    aConnection.Free;
  end;
end;

procedure TForm1.DBOpen;
begin
  csvCnn := TmncCSVConnection.Create;
  Connection := TmncFBConnection.Create;
  Connection.Resource := 'd:\temp\data.fdb';
  //Connection.CharacterSet := 'WIN1252';
  Connection.CharacterSet := 'NONE';
  Connection.UserName := 'sysdba';
  Connection.Password := 'masterkey';
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
begin
  Cancel := False;
  DBOpen;
  try
    for i := 0 to TablesList.Lines.Count - 1 do
    begin
      s:= trim(TablesList.Lines[i]);
      Count := 0;
      if s <> '' then
      begin
        try
          ExportTable(s);
        except
          on E: Exception do
            ResultMemo.Lines.Add(E.Message);
        end;

        ResultMemo.Lines.Add(s + ' is exported: ' + IntToStr(Count));
        ResultMemo.Lines.Add('---------------------');
        Application.ProcessMessages;
      end;
    end;
  finally
    DBClose;
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
  aStream := TFileStream.Create('d:\temp\data\'+TableName+'.csv', fmCreate);
  try
    CMD := Session.CreateCommand as TmncFBCommand;
    CMD.SQL.Text := 'select * from "'+TableName+'"';

    CMD.Prepare;

    csvCMD := TmncCSVCommand.Create(csvSes, aStream, csvmWrite);
    try

      for i := 0 to CMD.Columns.Count -1 do
        csvCMD.Columns.Add(CMD.Columns[i].Name, dtString);

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
  //ExportIt;
  ExportAll;
end;

end.

