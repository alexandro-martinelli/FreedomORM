unit AM.Freedom.CustomFreedomBindKeyBoard;

interface

uses
  ToolsAPI,
  AM.Freedom.Exceptions,
  System.Classes;

type
  TShortCutArray = array of TShortCut;
  TCustomFreedomBindKeyBoard = class(TNotifierObject, IUnknown, IOTANotifier, IOTAKeyboardBinding)
  strict private
    procedure BindKeyBoardProc(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult); virtual;
    function GetBindingType: TBindingType; virtual;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  protected
    function GetDisplayName: string; virtual;
    function GetName: string; virtual;
    procedure Execute(const Context: IOTAKeyContext; KeyCode: TShortCut); virtual;
    function GetShortCuts: TShortCutArray; virtual;
  end;

implementation

{ TGroupCriteriaBindKeyBoard }

procedure TCustomFreedomBindKeyBoard.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding(GetShortCuts, BindKeyBoardProc, nil);
end;

function TCustomFreedomBindKeyBoard.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TCustomFreedomBindKeyBoard.GetDisplayName: string;
begin
  Result := '';
end;

function TCustomFreedomBindKeyBoard.GetName: string;
begin
  Result := '';
end;

function TCustomFreedomBindKeyBoard.GetShortCuts: TShortCutArray;
begin
  raise EInvalidMethodCallOnClass.Create('GetShortCuts', ClassName);
end;

procedure TCustomFreedomBindKeyBoard.BindKeyBoardProc(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
  Execute(Context, KeyCode);
  BindingResult := krHandled;
end;

procedure TCustomFreedomBindKeyBoard.Execute(const Context: IOTAKeyContext; KeyCode: TShortCut);
begin
  raise EInvalidMethodCallOnClass.Create('Execute', ClassName);
end;

end.
