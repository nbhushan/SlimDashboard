#define MyAppName "slimDashboard"
#define MyAppVersion "0.0.0"
#define MyAppExeName "slimDashboard.bat"
#define RVersion "3.3.2"
#define IncludeR true
#define MyAppPublisher ""
#define MyAppURL ""


[Setup]
AppId = {{S1DOA2KZ-NE6O-JTB8-XQLY-H2RNG0Z7RVYW}
AppName = {#MyAppName}
DefaultDirName = {pf}\{#MyAppName}
DefaultGroupName = {#MyAppName}
OutputDir = wizard
OutputBaseFilename = setup_{#MyAppName}
SetupIconFile = setup.ico
AppVersion = {#MyAppVersion}
AppPublisher = {#MyAppPublisher}
AppPublisherURL = {#MyAppURL}
AppSupportURL = {#MyAppURL}
AppUpdatesURL = {#MyAppURL}
PrivilegesRequired = lowest
InfoBeforeFile = infobefore.txt
Compression = lzma2/ultra64
SolidCompression = yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"

[Files]
Source: "{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion
#if IncludeR
    Source: "R-{#RVersion}-win.exe"; DestDir: "{tmp}"; Check: RNeeded
#endif
Source: "crosshair.js"; DestDir: "{app}"; Flags: ignoreversion;
Source: "Dashboard.Rproj"; DestDir: "{app}"; Flags: ignoreversion;
Source: "default.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "helpers.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "installer.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "LICENSE"; DestDir: "{app}"; Flags: ignoreversion;
Source: "R-3.3.2-win.exe"; DestDir: "{app}"; Flags: ignoreversion;
Source: "README.md"; DestDir: "{app}"; Flags: ignoreversion;
Source: "server.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "setup.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "slimDashboard.bat"; DestDir: "{app}"; Flags: ignoreversion;
Source: "test.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "ui.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "RInno_installer\setup_slimDashboard.exe"; DestDir: "{app}\RInno_installer"; Flags: ignoreversion;
Source: "utils\app.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\config.cfg"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\ensure.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\get_app_from_app_url.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\package_manager.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\wsf\js\JSON.minify.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils\wsf\js\json2.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils\wsf\js\run.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils\wsf\run.wsf"; DestDir: "{app}\utils\wsf"; Flags: ignoreversion;

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\default.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\default.ico"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon; IconFilename: "{app}\default.ico"

[Run]
#if IncludeR
	Filename: "{tmp}\R-{#RVersion}-win.exe"; Parameters: "/SILENT"; WorkingDir: {tmp}; Flags: skipifdoesntexist; StatusMsg: "Installing R if needed"
#endif
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: shellexec postinstall skipifsilent


[UninstallDelete]
Type: filesandordirs; Name: "{app}\library";
Type: filesandordirs; Name: "{app}\utils";
Type: filesandordirs; Name: "{app}\log";

[Code]
const
  RRegKey = 'Software\R-Core\R\{#RVersion}';
  ChromeRegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\chrome.exe';
  IERegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\IEXPLORE.EXE';
  FFRegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\firefox.exe';
var
  RegPathsFile: string;

// Is R installed?
function RDetected(): boolean;
var
    success: boolean;
begin
  success := RegKeyExists(HKLM, RRegKey) or RegKeyExists(HKCU, RRegKey);
  begin
    Result := success;
  end;
end;

// If R is not detected, it is needed
function RNeeded(): Boolean;
begin
  Result := (RDetected = false);
end;

// Registry path update function (adds an extra backslash for json)
function AddBackSlash(Value: string): string;
begin
  Result := Value;
  StringChangeEx(Result, '\', '\\', True);
end;

// Save installation paths
procedure CurStepChanged(CurStep: TSetupStep);
var
  RPath: string;
  ChromePath: string;
  IEPath: string;
  FFPath: string;
begin
if CurStep = ssPostInstall then begin
    RPath := '';
    ChromePath := '';
    IEPath := '';
    FFPath := '';
    RegPathsFile := ExpandConstant('{app}\utils\regpaths.json');
    // Create registry paths file
    SaveStringToFile(RegPathsFile, '{' + #13#10, True);

    // R RegPath
    if RegQueryStringValue(HKLM, RRegKey, 'InstallPath', RPath) or RegQueryStringValue(HKCU, RRegKey, 'InstallPath', RPath) then
      SaveStringToFile(RegPathsFile, '"r": "' + AddBackSlash(RPath) + '",' + #13#10, True)
    else
      SaveStringToFile(RegPathsFile, '"r": "none",' + #13#10, True);

    // Chrome RegPath
    if RegQueryStringValue(HKLM, ChromeRegKey, 'Path', ChromePath) then
      SaveStringToFile(RegPathsFile, '"chrome": "' + AddBackSlash(ChromePath) + '",' + #13#10, True)
    else
      SaveStringToFile(RegPathsFile, '"chrome": "none",' + #13#10, True);

    // Internet Explorer RegPath
    if RegQueryStringValue(HKLM, IERegKey, '', IEPath) then
      SaveStringToFile(RegPathsFile, '"ie": "' + AddBackSlash(IEPath) + '",' + #13#10, True)
    else
      SaveStringToFile(RegPathsFile, '"ie": "none",' + #13#10, True);

    // Firefox RegPath
    // ** Last Line in json file (no trailing comma) **
    if RegQueryStringValue(HKLM, FFRegKey, 'Path', FFPath) then
      SaveStringToFile(RegPathsFile, '"ff": "' + AddBackSlash(FFPath) + '"' + #13#10, True)
    else
      SaveStringToFile(RegPathsFile, '"ff": "none"' + #13#10, True);

    SaveStringToFile(RegPathsFile, '}', True);
  end;
end;

