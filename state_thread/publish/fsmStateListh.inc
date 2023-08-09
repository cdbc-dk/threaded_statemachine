{%MainUnit fsm_engine.pas}
{ (c)2023 Benny Christensen a.k.a. cdbc }

  { TfsmStateList is an internal list of registered states, used in TbcStateMachine }
  TfsmStateList = class
  private
    fList: TFPObjectList;
    fOwnsObjects: boolean;
    function get_Count: integer;
    function get_Item(Index: integer): TFiniteState;
    procedure set_Item(Index: integer;aValue: TFiniteState);
  public
    constructor Create(OwnsObjects: boolean = true);
    destructor Destroy; override;
    function AddState(aState: TFiniteState): integer;
    procedure Clear;
    function Contains(aClass: TFiniteStateClass): boolean;
    function GetStateByClass(aClass: TFiniteStateClass): TFiniteState;
    function GetStateByName(const aName: string): TFiniteState;
    property Count: integer read get_Count;
    property Items[Index: integer]: TFiniteState read get_Item write set_Item; default;
  end; { TfsmStateList }
  
// included by fsm_engine.pas
