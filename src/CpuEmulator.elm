module CpuEmulator exposing (main)


import Array exposing (Array)
import Binary
import Bitwise
import List.Extra
import Html exposing (Html)
import Browser
import Element as E
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Assembler


type alias Model =
  { computer : Computer
  , editingInstructionIndex : Maybe Int
  }


type Msg
  = StepComputer
  | StartEditingInstruction Int
  | EditInstruction Int String
  | StopEditingInstruction Int


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }


type alias Computer =
  { aRegister : Int
  , dRegister : Int
  , mRegister : Int
  , pc : Int
  , ram : Array Int
  , rom : Array String
  , error : Maybe (Int, String)
  }


type alias Memory =
  Array Int


type alias Bit =
  Bool


colors =
  { lightGreen =
    E.rgb255 102 255 102
  , lightGrey =
    E.rgb255 220 220 220
  , darkGrey =
    E.rgb255 200 200 200
  , lightOrange =
    E.rgb255 255 165 0
  }


styles =
  { button =
    [ Background.color colors.lightGrey
    , E.mouseOver
      [ Background.color colors.darkGrey ]
    , E.paddingXY 15 10
    ]
  }


init : Model
init =
  let
    program =
      [ "@2"
      , "D=A-2"
      , "@3"
      , "D=D+A"
      , "@0"
      , "M=D"
      ]
  in
  { computer =
    { aRegister = 0
    , dRegister = 0
    , mRegister = 0
    , pc = 0
    , rom = Array.append
      (Array.fromList program)
      (Array.repeat (2 ^ 20 - List.length program) "")
    , ram = Array.repeat (2 ^ 21) 0
    , error = Nothing
    }
  , editingInstructionIndex =
    Nothing
  }


view : Model -> Html Msg
view model =
  E.layout
    [ E.width E.fill
    , E.height E.fill
    , E.padding 20
    ] <|
    E.row
      [ E.width E.fill
      , E.spacing 20
      ]
      [ E.column
        [ E.spacing 20
        , E.alignTop
        ]
        [ viewRAM model.computer
        , E.column
          [ E.width E.fill
          , E.spacing 10
          ]
          [ viewRegister "A" model.computer.aRegister
          , viewRegister "D" model.computer.dRegister
          , viewRegister "M" model.computer.mRegister
          ]
        ]
      , E.column
        [ E.spacing 20
        , E.alignTop
        ]
        [ viewROM model
        , E.column
          [ E.width E.fill
          , E.spacing 10
          ]
          [ viewRegister "PC" model.computer.pc
          , viewStepControl model.computer
          , viewAssemblerErrorMessage model.computer.error
          ]
        ]
      ]


viewROM : Model -> E.Element Msg
viewROM model =
  let
    instructionData =
      Array.toList <| Array.slice 0 28 model.computer.rom
  in
  E.column
    [ E.width <| E.px 200
    ] <|
    [ E.text "ROM"
    , E.indexedTable []
      { data = instructionData
      , columns =
          [ { header = E.none
            , width = E.px 50
            , view =
              \index _ ->
                E.el
                [] <|
                E.text <| String.fromInt index
          }
          , { header = E.none
            , width = E.fill
            , view =
                \index cell ->
                  let
                    commonStyle =
                      [ E.paddingXY 10 0
                      , Border.width 1
                      , E.height <| E.px 22
                      ]

                    cellStyle =
                      ( if index == model.computer.pc then
                          commonStyle
                          ++ [ Background.color colors.lightGreen
                          ]
                        else
                          commonStyle
                      )
                      |>
                      (\previousStyle ->
                        case model.computer.error of
                          Nothing ->
                            previousStyle
                          
                          Just (location, _) ->
                            if location == index then
                              previousStyle
                              ++ [ Background.color colors.lightOrange
                              ]
                            else
                              previousStyle
                      )

                    isEditing =
                      case model.editingInstructionIndex of
                        Nothing ->
                          False
                        
                        Just editingIndex ->
                          index == editingIndex
                  in
                  E.el cellStyle <|
                  if isEditing then
                    Input.text
                      (cellStyle
                      ++ [ Events.onLoseFocus <| StopEditingInstruction index
                      ])
                      { onChange =
                        EditInstruction index
                      , text =
                        cell
                      , placeholder =
                        Nothing
                      , label =
                        Input.labelHidden "edit instruction"
                      }
                  else
                    E.el
                    [ Events.onClick <| StartEditingInstruction index
                    , E.width E.fill
                    ] <|
                    E.text cell
            }
          ]
      }
    ]


viewRAM : Computer -> E.Element Msg
viewRAM computer =
  viewMemory "RAM" 0 computer.ram


viewRegister : String -> Int -> E.Element Msg
viewRegister name value =
  E.el
  [ Border.width 2
  , E.width E.fill
  , E.padding 5
  ] <|
  E.text <| name ++ " = " ++ String.fromInt value


viewMemory : String -> Int -> Memory -> E.Element Msg
viewMemory name highlightedAddress memory =
  let
    memoryData =
      Array.toList <| Array.slice 0 28 memory
  in
  E.column
    [ E.width <| E.px 200
    ] <|
    [ E.text name
    , E.indexedTable []
      { data = memoryData
      , columns =
          [ { header = E.none
            , width = E.px 50
            , view =
              \index _ ->
                E.el
                [] <|
                E.text <| String.fromInt index
          }
          , { header = E.none
            , width = E.fill
            , view =
                \index cell ->
                  let
                    cellStyle =
                      if index == highlightedAddress then
                        [ Background.color colors.lightGreen
                        ]
                      else
                        []
                  in
                  E.el
                  (cellStyle
                  ++ [ E.paddingXY 10 0
                  , Border.width 1
                  ])
                  <|
                  E.text <| String.fromInt cell
            }
          ]
      }
    ]

viewStepControl : Computer -> E.Element Msg
viewStepControl computer =
  E.row []
    [ Input.button styles.button
      { onPress =
        Just StepComputer
      , label =
        E.text ">"
      }
    ]


viewAssemblerErrorMessage : Maybe (Int, String) -> E.Element Msg
viewAssemblerErrorMessage errorMessage =
  case errorMessage of
    Nothing ->
      E.none
    
    Just (_, msg) ->
      E.el
      [ E.width <| E.px 30 -- used to make sure other elements' widths are not expanded
      ]
      <| E.text <| "❌ " ++ msg


update : Msg -> Model -> Model
update msg model =
  case msg of
    StepComputer ->
      { model
        | computer =
          step model.computer
      }

    StartEditingInstruction index ->
      startEditingInstruction index model

    EditInstruction index newInstruction ->
      editInstruction index newInstruction model

    StopEditingInstruction index ->
      stopEditingInstruction index model


stopEditingInstruction : Int -> Model -> Model
stopEditingInstruction index model =
  { model
    | editingInstructionIndex =
      Nothing
  }


editInstruction : Int -> String -> Model -> Model
editInstruction index newInstruction model =
  let
    oldComputer =
      model.computer
  in
  { model
    | computer =
      { oldComputer
        | rom =
          Array.set index newInstruction oldComputer.rom
      }
  }


startEditingInstruction : Int -> Model -> Model
startEditingInstruction index model =
  { model
    | editingInstructionIndex =
      Just index
  }


step : Computer -> Computer
step computer =
  case Array.get computer.pc computer.rom of
    Just instructionStr ->
      case Assembler.assembleInstruction computer.pc instructionStr of
        Err err ->
          { computer
            | error =
              Just <| (computer.pc, err)
          }
        
        Ok instruction ->
          let
            computer1 =
              case computer.error of
                Nothing ->
                  computer
                
                Just _ ->
                  { computer
                    | error =
                      Nothing
                  }

            instructionBinary =
              Binary.fromIntegers <| List.map (Maybe.withDefault 0 << String.toInt) <| String.split "" instruction
            
            instructionBits =
              Binary.toBooleans <| instructionBinary
          in
          case instructionBits of
            [] ->
              computer1
            
            opCode :: _ ->
              if not opCode then
                stepAInstruction (Binary.toDecimal instructionBinary) computer1
              else
                stepCInstruction (List.drop 19 instructionBits) computer1
    
    Nothing ->
      computer


stepAInstruction : Int -> Computer -> Computer
stepAInstruction number computer =
  { computer
    | pc =
      computer.pc + 1
    , aRegister =
      number
    , mRegister =
      fetchFromMemory number computer.ram
  }


-- 13 instruction bits in the order of
-- computation, destinations, and jump
stepCInstruction : List Bit -> Computer -> Computer
stepCInstruction instructionBits computer =
  let
    computationBits =
      List.take 7 instructionBits
    
    destinationsBits =
      List.take 3 <| List.drop 7 instructionBits
    
    jumpBits =
      List.take 3 <| List.drop 10 instructionBits
    
    computationResult =
      compute computationBits computer
  in
  moveProgramCounter jumpBits computationResult  <|
  storeComputationResult destinationsBits computationResult <|
  computer

compute : List Bit -> Computer -> Int
compute computationBits computer =
  let
    computationStr =
      String.join "" <| List.map boolToString <| computationBits
    
    d =
      computer.dRegister
    
    a =
      computer.aRegister

    m =
      computer.mRegister
  in
  case computationStr of
    "0101010" ->
      0
    
    "0111111" ->
      1
    
    "0111010" ->
      -1
    
    "0001100" ->
      d
    
    "0110000" ->
      a
    
    "0001101" ->
      Bitwise.complement d
    
    "0110001" ->
      Bitwise.complement a
    
    "0001111" ->
      -d
    
    "0110011" ->
      -a
    
    "0011111" ->
      d + 1
    
    "0110111" ->
      a + 1
    
    "0001110" ->
      d - 1
    
    "0110010" ->
      a - 1
    
    "0000010" ->
      d + a

    "0010011" ->
      d - a
    
    "0000111" ->
      a - d
    
    "0000000" ->
      Bitwise.and d a
    
    "0010101" ->
      Bitwise.or d a
    
    "1110000" ->
      m

    "1110001" ->
      Bitwise.complement m
    
    "1110011" ->
      -m

    "1110111" ->
      m + 1
    
    "1110010" ->
      m - 1
    
    "1000010" ->
      d + m
    
    "1010011" ->
      d - m
    
    "1000111" ->
      m - d
    
    "1000000" ->
      Bitwise.and d m
    
    "1010101" ->
      Bitwise.or d m
    
    _ ->
      0 -- invalid computation bits


storeComputationResult : List Bit -> Int -> Computer -> Computer
storeComputationResult destinationsBits result computer =
  let
    storeToARegister =
      getBit 0 destinationsBits
    
    newARegister =
      if storeToARegister then result else computer.aRegister
    
    storeToDRegister =
      getBit 1 destinationsBits
  
    newDRegister =
      if storeToDRegister then result else computer.dRegister

    storeToMRegister =
      getBit 2 destinationsBits
    
    newMRegister =
      if storeToMRegister then result else computer.mRegister
  in
  { computer
    | aRegister =
      newARegister
    , dRegister =
      newDRegister
    , mRegister =
      newMRegister
    , ram =
      storeToMemory computer.aRegister newMRegister computer.ram
  }


moveProgramCounter : List Bit -> Int -> Computer -> Computer
moveProgramCounter jumpBits computationResult computer =
  let
    zr =
      computationResult == 0
    
    ng =
      computationResult < 0
    
    lt =
      getBit 0 jumpBits
    
    eq =
      getBit 1 jumpBits
    
    gt =
      getBit 2 jumpBits

    jump =
      (if lt then ng else False)
      || (if eq then zr else False)
      || (if gt then not ng && not zr else False)
  in
  if jump then
    { computer
      | pc =
        computer.aRegister
    }
  else
    { computer
      | pc =
        computer.pc + 1
    }


fetchFromMemory : Int -> Memory -> Int
fetchFromMemory address memory =
  Maybe.withDefault 0 <| Array.get address memory


storeToMemory : Int -> Int -> Memory -> Memory
storeToMemory address value memory =
  Array.set address value memory


getBit : Int -> List Bit -> Bit
getBit index bits =
  Maybe.withDefault False <| List.Extra.getAt index bits


boolToString : Bool -> String
boolToString b =
  if b then "1" else "0"