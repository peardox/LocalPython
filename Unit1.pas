unit Unit1;

 {$DEFINE USELOCAL}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, PyEnvironment,
  PyEnvironment.Local, PythonEngine, PyEnvironment.Embeddable,
  PyEnvironment.Embeddable.Res, PyEnvironment.Embeddable.Res.Python39,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.PythonGUIInputOutput, PyCommon, PyModule, PyPackage, PSUtil, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    PyEng: TPythonEngine;
    PyIO: TPythonGUIInputOutput;
    Memo1: TMemo;
    PSUtil: TPSUtil;
    PyEmbed: TPyEmbeddedResEnvironment39;
    Panel1: TPanel;
    Button1: TButton;
    PyLocal: TPyLocalEnvironment;
    procedure PyEmbedAfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PSUtilAfterImport(Sender: TObject);
    procedure PSUtilAfterInstall(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PyEmbedAfterSetup(Sender: TObject; const APythonVersion: string);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    PyIsActivated: Boolean;
    procedure WriteLocalPythonJSON(const outfile: String; const version: String; const pythonpath: String; const libfile: String; const exefile: String);
    procedure Test;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  JSONFileName = 'python.json';
  AppName = 'SimplePython';
  PyVersion = '3.9';

implementation

{$R *.fmx}

procedure TForm1.Test;
var
  SomeCode: TStringList;
  cores: Variant;
  threads: Variant;
  memory: Variant;
begin
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Some simple Python...');
  SomeCode := TStringList.Create;
  SomeCode.DefaultEncoding := TEncoding.ANSI;
  try
    SomeCode.Add('import sys');
    SomeCode.Add('import io');
    SomeCode.Add('for p in sys.path:');
    SomeCode.Add('  print(p)');

    PyEng.ExecString(SomeCode.Text);

  finally
    SomeCode.Free;
  end;

  if PSUtil.IsImported then
    begin
      Memo1.Lines.Add('');

      cores := PSUtil.psutil.cpu_count(False);
      threads := PSUtil.psutil.cpu_count(True);
      memory := PSUtil.psutil.virtual_memory();

      Memo1.Lines.Add('Show some info from PSUtil...');
      Memo1.Lines.Add('PSUtil says this PC has ' +
        cores + ' cores, ' +
        threads + ' threads' +
        ' and ' + memory.total + ' bytes or memory');
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
{$IFDEF USELOCAL}
var
  OutFile: String;
  PyPath: String;
{$ENDIF}
begin
  if not PyIsActivated then
    begin
      OutFile := '';
      {$IFDEF USELOCAL}
      // MacOSX with X64 CPU
      {$IF DEFINED(MACOS64) AND DEFINED(CPUX64)}
      OutFile := IncludeTrailingPathDelimiter(
                  IncludeTrailingPathDelimiter(
                  System.IOUtils.TPath.GetLibraryPath) +
                  AppName) +
                  JSONFileName;
      PyPath := '/Library/Frameworks/Python.framework/Versions/3.9';
      WriteLocalPythonJSON(OutFile, PyVersion, PyPath,
        TPath.Combine(PyPath, 'lib/libpython3.9.dylib'),
        TPath.Combine(PyPath, 'bin/python3.9'));
      // MacOSX with ARM64 CPU (M1 etc)
      {$ELSEIF DEFINED(MACOS64) AND DEFINED(CPUARM64)}
      OutFile := IncludeTrailingPathDelimiter(
                  IncludeTrailingPathDelimiter(
                  System.IOUtils.TPath.GetLibraryPath) +
                  AppName) +
                  JSONFileName;
      PyPath := '/Library/Frameworks/Python.framework/Versions/3.9';
      WriteLocalPythonJSON(OutFile, PyVersion, PyPath,
        TPath.Combine(PyPath, 'lib/libpython3.9.dylib'),
        TPath.Combine(PyPath, 'bin/python3.9'));
      // Windows X64 CPU
      {$ELSEIF DEFINED(WIN64)}
      OutFile := IncludeTrailingPathDelimiter(
                  IncludeTrailingPathDelimiter(
                  System.IOUtils.TPath.GetHomePath) +
                  AppName) +
                  JSONFileName;
      PyPath := ExpandFileName(TPath.Combine(TPath.GetHomePath, '..\Local\Programs\Python\Python39'));
      WriteLocalPythonJSON(OutFile, PyVersion, PyPath.Replace('\','\\'),
        TPath.Combine(PyPath, 'python39.dll').Replace('\','\\'),
        TPath.Combine(PyPath, 'python.exe').Replace('\','\\'));
      // Windows 32 bit
      {$ELSEIF DEFINED(WIN32)}
      OutFile := IncludeTrailingPathDelimiter(
                  IncludeTrailingPathDelimiter(
                  System.IOUtils.TPath.GetHomePath) +
                  AppName + '-32') +
                  JSONFileName;
      // Linux X64 CPU
      {$ELSEIF DEFINED(LINUX64)}
      OutFile := IncludeTrailingPathDelimiter(
                  IncludeTrailingPathDelimiter(
                  System.IOUtils.TPath.GetHomePath) +
                  AppName) +
                  JSONFileName;
      // Android (64 CPU) Not presently working)
      {$ELSEIF DEFINED(ANDROID)}
      OutFile := IncludeTrailingPathDelimiter(
                  IncludeTrailingPathDelimiter(
                  System.IOUtils.TPath.GetHomePath) +
                  AppName) +
                  JSONFileName;
      {$ELSE}
      raise Exception.Create('Need to set OutFile for this build');
      {$ENDIF}
      if OutFile = '' then
        begin
          ShowMessage('Can''t create JSON file');
          exit;
        end;
      PyLocal.FilePath := OutFile;
      PyLocal.PythonVersion := '3.9';
      PyLocal.Setup(PyLocal.PythonVersion);
      if not PyIsActivated then
        ShowMessage('Python was not set up');
      {$ELSE}
      PyEmbed.Setup(PyEmbed.PythonVersion);
      if not PyIsActivated then
        ShowMessage('Python was not set up');
      {$ENDIF}
      Button1.Text := 'Run Python'
    end
  else
    Test;
end;

procedure TForm1.WriteLocalPythonJSON(const outfile: String; const version: String; const pythonpath: String; const libfile: String; const exefile: String);
var
  SW: TStreamWriter;
  JSONText: String;
begin
  SW := Nil;
  JSONText := '[{' +
    '"' + version + '": {' +
    '"home":"' + pythonpath + '",' +
    '"shared_library":"' + libfile +'",' +
    '"executable":"'+ exefile + '"' +
    '}}]';
//  if not FileExists(outfile) then
    begin
      try
        try
          if not DirectoryExists(ExtractFilePath(outfile)) then
            ForceDirectories(ExtractFilePath(outfile));
          SW := TStreamWriter.Create(outfile);
          SW.Write(JSONText);
        except
          on E: Exception do
            begin
              Memo1.Lines.Add('Unhandled Exception');
              Memo1.Lines.Add('Class : ' + E.ClassName);
              Memo1.Lines.Add('Error : ' + E.Message);
            end;
        end;
      finally
        SW.Free;
      end;
    end;
end;

procedure TForm1.PyEmbedAfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  {$IFDEF USELOCAL}
  PyLocal.Activate(PyLocal.PythonVersion);
  {$ELSE}
  PyEmbed.Activate(PyEmbed.PythonVersion);
  {$ENDIF}
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF USELOCAL}
  Form1.Caption := 'Using Local Python';
{$ELSE}
  Form1.Caption := 'Using Embedded Python';
{$ENDIF}
  PyIsActivated := False;
  // This needs changing when LocalEnvironment works - it's only cosmetic anyway
  if DirectoryExists(TPath.Combine(PyEmbed.EnvironmentPath, PyEmbed.PythonVersion)) then
    Button1.Text := 'Run Python'
  else
    Button1.Text := 'Setup Python';
end;

procedure TForm1.PSUtilAfterImport(Sender: TObject);
begin
  Memo1.Lines.Add('PSUtil has imported');
  Test;
  PyIsActivated := True;
end;

procedure TForm1.PSUtilAfterInstall(Sender: TObject);
begin
  Memo1.Lines.Add('PSUtil has installed');
end;

procedure TForm1.PyEmbedAfterActivate(Sender: TObject;
  const APythonVersion: string; const AActivated: Boolean);
begin
  Memo1.Lines.Add('Python is active');
  if Not PSUtil.IsInstalled then
    PSUtil.Install;
  PSUtil.Import;
end;

end.
