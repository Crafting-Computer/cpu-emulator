port module CpuEmulator exposing (main)


import Array exposing (Array)
import Binary
import Bitwise
import List.Extra
import Html exposing (Html)
import Html.Attributes
import Browser
import Browser.Dom
import Browser.Events
import Task
import Element as E
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Assembler
import InfiniteScroll


port showProgramEditorPort : () -> Cmd msg
port hideProgramEditorPort : () -> Cmd msg
port editProgramPort : (String -> msg) -> Sub msg
port showAssemblerErrorPort : ((Int, Int), String) -> Cmd msg
port clearAssemblerErrorPort : () -> Cmd msg
port scrollIntoViewPort : String -> Cmd msg
port updatePixelsPort : List Pixel -> Cmd msg


type alias Model =
  { computer : Computer
  , editingInstructionIndex : Maybe Int
  , editingRamIndex : Maybe Int
  , assemblerError : Maybe String
  , isEditingProgram : Bool
  , program : String
  , instructions : Array String
  , isRunningComputer : Bool
  , ramScroll : InfiniteScroll.Model Msg
  , ramDisplaySize : Int
  , romScroll : InfiniteScroll.Model Msg
  , romDisplaySize : Int
  , isAnimated : Bool
  }


type Msg
  = StepComputerOneFrame Float
  | StepComputer
  | StartRunningComputer
  | StopRunningComputer
  | ResetComputer
  | StartEditingInstruction Int
  | EditInstruction Int String
  | StopEditingInstruction Int
  | StartEditingProgram
  | EditProgram String
  | StopEditingProgram
  | StartEditingRam Int
  | EditRam Int String
  | StopEditingRam Int
  | RamScrollMsg InfiniteScroll.Msg
  | LoadedMoreRam
  | RomScrollMsg InfiniteScroll.Msg
  | LoadedMoreRom
  | NoOp


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Computer =
  { aRegister : Int
  , dRegister : Int
  , mRegister : Int
  , pc : Int
  , ram : Array Int
  , rom : Array Int
  , error : Maybe (Int, String)
  , updatedPixels : List Pixel
  }


type alias Pixel =
  ((Int, Int), Int)


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


init : () -> (Model, Cmd Msg)
init _ =
  ( { computer =
    { aRegister = 0
    , dRegister = 0
    , mRegister = 0
    , pc = 0
    , rom = Array.repeat (2 ^ 18) 0
    , ram = Array.repeat (2 ^ 18) 0
    , error = Nothing
    , updatedPixels = []
    }
  , editingInstructionIndex =
    Nothing
  , editingRamIndex =
    Nothing
  , assemblerError =
    Nothing
  , isEditingProgram =
    False
  , program =
    ""
  , instructions =
    Array.repeat (2 ^ 18) ""
  , isRunningComputer =
    False
  , ramScroll =
    InfiniteScroll.init loadMoreRam
  , ramDisplaySize =
    50
  , romScroll =
    InfiniteScroll.init loadMoreRom
  , romDisplaySize =
    50
  , isAnimated =
    True
  }
  , Cmd.none
  )


loadMoreRam : InfiniteScroll.Direction -> Cmd Msg
loadMoreRam _ =
  msgToCmd LoadedMoreRam


loadMoreRom : InfiniteScroll.Direction -> Cmd Msg
loadMoreRom _ =
  msgToCmd LoadedMoreRom


msgToCmd : msg -> Cmd msg
msgToCmd x =
    Task.perform identity (Task.succeed x)


view : Model -> Html Msg
view model =
  E.layout
    [ E.width E.fill
    , E.height E.fill
    , E.padding 20
    , Font.family
      [ Font.typeface "Roboto Mono"
      , Font.monospace
      ]
    ] <|
    E.row
      [ E.width E.fill
      , E.spacing 20
      ]
      [ E.column
        [ E.spacing 20
        , E.alignTop
        ]
        [ viewRam model
        , E.column
          [ E.width E.fill
          , E.spacing 10
          ]
          [ viewRegister "A" model.computer.aRegister model.isAnimated
          , viewRegister "D" model.computer.dRegister model.isAnimated
          , viewRegister "M" model.computer.mRegister model.isAnimated
          ]
        ]
      , E.column
        [ E.spacing 20
        , E.alignTop
        ]
        [ viewRom model
        , E.column
          [ E.width E.fill
          , E.spacing 10
          , E.onRight <| viewAssemblerErrorMessage model.computer.error
          ]
          [ viewRegister "PC" model.computer.pc model.isAnimated
          , E.row
            [ E.spacing 20 ]
            [ viewSingleStepButton
            , viewRunButton model.isRunningComputer
            , viewResetButton
            ]
          , E.row
            [ E.spacing 20 ]
            [ viewEditButton model.computer
            ]
          ]
        ]
      , viewCloseEditorButton model.isEditingProgram
      ]


viewCloseEditorButton : Bool -> E.Element Msg
viewCloseEditorButton isEditingProgram =
  if isEditingProgram then
    Input.button
      ( styles.button
      ++ [ E.htmlAttribute <| Html.Attributes.style "position" "fixed"
      , E.htmlAttribute <| Html.Attributes.style "bottom" "10px"
      , E.htmlAttribute <| Html.Attributes.style "left" "0px"
      ]
      )
      { onPress =
        Just StopEditingProgram
      , label =
        E.text "Save and Close"
      }
  else
    E.none


viewRom : Model -> E.Element Msg
viewRom model =
  let
    instructionData =
      Array.toList <| Array.slice 0 model.romDisplaySize model.instructions
  in
  E.column
    [ E.width <| E.px 200
    , E.htmlAttribute <| Html.Attributes.style "height" "640px"
    ] <|
    [ E.text "ROM"
    , indexedTable
      [ E.htmlAttribute <| InfiniteScroll.infiniteScroll RomScrollMsg
      , E.htmlAttribute <| Html.Attributes.style "height" "640px"
      , E.htmlAttribute <| Html.Attributes.style "overflow-y" "auto"
      ]
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
                      , E.width <| E.px 130
                      , E.htmlAttribute <| Html.Attributes.id <| "instruction" ++ String.fromInt index
                      ]

                    cellStyle =
                      ( if model.isAnimated && index == model.computer.pc then
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


viewRegister : String -> Int -> Bool -> E.Element Msg
viewRegister name value isAnimated =
  E.el
  [ Border.width 2
  , E.width E.fill
  , E.padding 5
  ] <|
  E.text <| name ++ " = "
  ++ ( if isAnimated then
    String.fromInt value
  else
    "-"
  )


viewRam : Model -> E.Element Msg
viewRam model =
  let
    memoryData =
      Array.toList <| Array.slice 0 model.ramDisplaySize model.computer.ram
  in
  E.column
    [ E.width <| E.px 200
    , E.htmlAttribute <| Html.Attributes.style "height" "640px"
    ] <|
    [ E.text "RAM"
    , indexedTable
      [ E.htmlAttribute <| InfiniteScroll.infiniteScroll RamScrollMsg
      , E.htmlAttribute <| Html.Attributes.style "height" "640px"
      , E.htmlAttribute <| Html.Attributes.style "overflow-y" "auto"
      ]
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
                    commonStyle =
                      [ E.paddingXY 10 0
                      , Border.width 1
                      , E.height <| E.px 22
                      , E.width <| E.px 130
                      ]
                    
                    cellStyle =
                      if index == 0 then
                        commonStyle
                        ++ [ Background.color colors.lightGreen
                        ]
                      else
                        commonStyle

                    isEditing =
                      case model.editingRamIndex of
                        Nothing ->
                          False
                        
                        Just editingIndex ->
                          index == editingIndex
                  in
                  E.el cellStyle <|
                  if isEditing then
                    Input.text
                      (cellStyle
                      ++ [ Events.onLoseFocus <| StopEditingRam index
                        , E.htmlAttribute <| Html.Attributes.id <| "ram" ++ String.fromInt index
                        , E.width E.fill
                      ])
                      { onChange =
                        EditRam index
                      , text =
                        String.fromInt cell
                      , placeholder =
                        Nothing
                      , label =
                        Input.labelHidden "edit ram"
                      }
                  else
                    E.el
                    [ Events.onClick <| StartEditingRam index
                    , E.width E.fill
                    ] <|
                    E.text <|
                    if model.isAnimated then
                      String.fromInt cell
                    else
                      "-"
            }
          ]
      }
    ]


indexedTable :
  List (E.Attribute Msg) ->
  { data : List record
  , columns : List (E.IndexedColumn record Msg)
  } -> E.Element Msg
indexedTable attributes { data, columns } =
  E.column
  ( attributes
    ++ [ E.width E.fill ]
  ) <|
  List.indexedMap
    (\index cell ->
      E.row [ E.width E.fill ] <|
      List.map
      (\column ->
        E.el
        [ E.width <| column.width
        ] <|
        column.view index cell
      )
      columns
    )
    data


viewSingleStepButton : E.Element Msg
viewSingleStepButton =
  Input.button styles.button
    { onPress =
      Just <| StepComputer
    , label =
      E.html
        (FeatherIcons.chevronRight
        |> FeatherIcons.toHtml []
        )
    }


viewRunButton : Bool -> E.Element Msg
viewRunButton isRunningComputer =
  Input.button styles.button
    { onPress =
      if isRunningComputer then
        Just StopRunningComputer
      else
        Just StartRunningComputer
    , label =
      E.html
        ( ( if isRunningComputer then
            FeatherIcons.pause
          else
            FeatherIcons.chevronsRight
        )
        |> FeatherIcons.toHtml []
        )
    }


viewResetButton : E.Element Msg
viewResetButton =
  Input.button styles.button
    { onPress =
      Just ResetComputer
    , label =
      E.html
        (FeatherIcons.chevronsLeft
        |> FeatherIcons.toHtml []
        )
    }


viewEditButton : Computer -> E.Element Msg
viewEditButton computer =
  Input.button styles.button
    { onPress =
      Just StartEditingProgram
    , label =
      E.html
        (FeatherIcons.edit
        |> FeatherIcons.toHtml []
        )
    }


viewAssemblerErrorMessage : Maybe (Int, String) -> E.Element Msg
viewAssemblerErrorMessage errorMessage =
  case errorMessage of
    Nothing ->
      E.none
    
    Just (location, msg) ->
      E.el
      [ E.paddingEach
        { left =
          20
        , right =
          0
        , top =
          0
        , bottom =
          0
        }
      -- , E.width <| E.px 30 -- used to make sure other elements' widths are not expanded
      , Font.letterSpacing -1
      ]
      <| E.html <|
        Html.pre
        [ Html.Attributes.style "white-space" "pre-wrap"
        , Html.Attributes.style "width" "50vw"
        , Html.Attributes.style "margin" "0"
        , Html.Attributes.style "line-height" "1.2em"
        ]
        [ Html.text <|
          "⚠️ I got stuck while assembling line " ++ String.fromInt location ++ ":\n"
          ++ msg
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StepComputer ->
      stepComputer model
    
    StepComputerOneFrame time ->
      stepComputerOneFrame time model

    StartRunningComputer ->
      startRunningComputer model

    StopRunningComputer ->
      stopRunningComputer model

    ResetComputer ->
      resetComputer model

    StartEditingInstruction index ->
      startEditingInstruction index model

    EditInstruction index newInstruction ->
      editInstruction index newInstruction model

    StopEditingInstruction index ->
      stopEditingInstruction index model

    StartEditingProgram ->
      startEditingProgram model

    EditProgram newProgram ->
      editProgram newProgram model

    StopEditingProgram ->
      stopEditingProgram model

    StartEditingRam index ->
      startEditingRam index model
    
    EditRam index newValue->
      editRam index newValue model
    
    StopEditingRam index ->
      stopEditingRam index model

    RamScrollMsg scrollMsg ->
      let
        ( nextRamScroll, cmd ) =
          InfiniteScroll.update RamScrollMsg scrollMsg model.ramScroll
      in
      ( { model | ramScroll = nextRamScroll }, cmd )

    LoadedMoreRam ->
      let
        nextRamScroll =
          InfiniteScroll.stopLoading model.ramScroll
      in
      ( { model | ramScroll = nextRamScroll, ramDisplaySize = model.ramDisplaySize + 200 }, Cmd.none )

    RomScrollMsg scrollMsg ->
      let
        ( nextRomScroll, cmd ) =
          InfiniteScroll.update RomScrollMsg scrollMsg model.romScroll
      in
      ( { model | romScroll = nextRomScroll }, cmd )

    LoadedMoreRom ->
      let
        nextRomScroll =
          InfiniteScroll.stopLoading model.romScroll
      in
      ( { model | romScroll = nextRomScroll, romDisplaySize = model.romDisplaySize + 200 }, Cmd.none )

    NoOp ->
      (model, Cmd.none)


resetComputer : Model -> (Model, Cmd Msg)
resetComputer model =
  let
    oldComputer =
      model.computer
  in
  ({ model
    | computer =
      { oldComputer
        | pc =
          0
      }
    , isRunningComputer =
      False
  }
  , scrollIntoViewPort "instruction0"
  )


stepComputer : Model -> (Model, Cmd Msg)
stepComputer model =
  let
    instructionId =
      "instruction" ++ String.fromInt model.computer.pc
  in
  ({ model
    | computer =
      step model.computer
  }
  , Cmd.batch
    [ if model.isAnimated then
      scrollIntoViewPort instructionId
    else
      Cmd.none
    , case model.computer.updatedPixels of
      [] ->
        Cmd.none
      
      pixels ->
        updatePixelsPort <| List.reverse pixels
    ]
  )


stepComputerOneFrame : Float -> Model -> (Model, Cmd Msg)
stepComputerOneFrame time model =
  let
    timeToRun =
      min time 1000
    
    steps =
      ceiling ((timeToRun / 1000) * 2.2 * 256 * 1024)
    
    nextComputer =
      stepComputerOneFrameHelper steps model.computer
  in
  ({ model
    | computer =
      nextComputer
  }
  , Cmd.batch
    [ case nextComputer.updatedPixels of
      [] ->
        Cmd.none
      
      pixels ->
        updatePixelsPort pixels
    ]
  )


stepComputerOneFrameHelper : Int -> Computer -> Computer
stepComputerOneFrameHelper numberOfInstructionsLeft computer =
  if numberOfInstructionsLeft > 0 then
    stepComputerOneFrameHelper (numberOfInstructionsLeft - 1) (step computer)
  else
    computer


startRunningComputer : Model -> (Model, Cmd Msg)
startRunningComputer model =
  ({ model
    | isRunningComputer =
      True
    , isAnimated =
      False
  }
  , Cmd.none
  )


stopRunningComputer : Model -> (Model, Cmd Msg)
stopRunningComputer model =
  ({ model
    | isRunningComputer =
      False
    , isAnimated =
      True
  }
  , Cmd.none
  )


startEditingRam : Int -> Model -> (Model, Cmd Msg)
startEditingRam index model =
  ({ model
    | editingRamIndex =
      Just index
  }
  , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus <| "ram" ++ String.fromInt index
  )


editRam : Int -> String -> Model -> (Model, Cmd Msg)
editRam index newValue model =
  let
    oldComputer =
      model.computer
  in
  ({ model
    | computer =
      { oldComputer
        | ram =
          storeToMemory index (Maybe.withDefault 0 <| String.toInt newValue) oldComputer.ram
      }
  }
  , Cmd.none
  )


stopEditingRam : Int -> Model -> (Model, Cmd Msg)
stopEditingRam index model =
  ({ model
    | editingRamIndex =
      Nothing
  }
  , Cmd.none
  )


editProgram : String -> Model -> (Model, Cmd Msg)
editProgram newProgram model =
  case Assembler.parseProgram newProgram of
    Ok _ ->
      ( { model
        | program =
          newProgram
        }
      , clearAssemblerErrorPort ()
      )

    Err error ->
      ( { model
        | program =
          newProgram
        , assemblerError =
          Just <| Tuple.second error
        }
      , showAssemblerErrorPort error
      )


startEditingProgram : Model -> (Model, Cmd Msg)
startEditingProgram model =
  ( { model
    | isEditingProgram =
      True
  }
  , showProgramEditorPort ()
  )


stopEditingProgram : Model -> (Model, Cmd Msg)
stopEditingProgram model =
  case Assembler.parseProgram model.program of
    Ok instructions ->
      let
        oldComputer =
          model.computer

        updatedPartOfInstructions =
          Array.fromList <|
            List.map Assembler.instructionToString instructions
        
        restOfOldInstructions =
          Array.slice (Array.length updatedPartOfInstructions) (Array.length model.instructions) model.instructions
        
        nextInstructions =
          Array.append updatedPartOfInstructions restOfOldInstructions

        updatedPartOfRom =
          Array.fromList <|
            Assembler.emitProgram instructions
        
        restOfOldRom =
          Array.slice (Array.length updatedPartOfRom) (Array.length oldComputer.rom) oldComputer.rom
        
        nextRom =
          Array.append updatedPartOfRom restOfOldRom
      in
      ({ model
        | instructions =
          nextInstructions
        , computer =
          { oldComputer
            | rom =
              nextRom
          }
        , isEditingProgram =
          False
      }
      , hideProgramEditorPort ()
      )
    
    Err _ ->
      ({ model
        | isEditingProgram =
          False
      }
      , hideProgramEditorPort ()
      )


-- updateInstructionsAndRom : List String -> Model -> Model
-- updateInstructionsAndRom newInstructions model =
--   let
--     oldComputer =
--       model.computer
--   in
--   { model
--     | instructions =
--       nextInstructions
--     , computer =
--     { oldComputer
--       | rom =
        
--     }
--   }


stopEditingInstruction : Int -> Model -> (Model, Cmd Msg)
stopEditingInstruction index model =
  let
    prevComputer =
      model.computer

    nextComputer =
      case Array.get prevComputer.pc model.instructions of
        Just instructionStr ->
          case Assembler.assembleInstruction prevComputer.pc instructionStr of
            Err err ->
              { prevComputer
                | error =
                  Just <| (prevComputer.pc, err)
              }
            
            Ok _ ->
              { prevComputer
                | error =
                  Nothing
              }

        Nothing ->
          prevComputer

  in  
  ({ model
    | computer =
      nextComputer
    , editingInstructionIndex =
      Nothing
  }
  , Cmd.none
  )


editInstruction : Int -> String -> Model -> (Model, Cmd Msg)
editInstruction index newInstruction model =
  ({ model
    | instructions =
      Array.set index newInstruction model.instructions
  }
  , Cmd.none
  )


startEditingInstruction : Int -> Model -> (Model, Cmd Msg)
startEditingInstruction index model =
  ({ model
    | editingInstructionIndex =
      Just index
  }
  , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus <| "instruction" ++ String.fromInt index
  )


step : Computer -> Computer
step computer =
  case Array.get computer.pc computer.rom of
    Just instruction ->
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

        opCode =
          getBit 31 instruction
      in
      if not opCode then
        stepAInstruction instruction computer1
      else
        stepCInstruction (dropBits 19 instruction) computer1
    
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
stepCInstruction : Int -> Computer -> Computer
stepCInstruction instruction computer =
  let
    computationBits =
      Bitwise.shiftRightZfBy 6 instruction -- take the first 7 bits
    
    destinationsBits =
      Bitwise.shiftRightZfBy 3 <| Bitwise.and 0x38 instruction -- take the next 3 bits
    
    jumpBits =
      Bitwise.and 0x7 instruction -- take the next 3 bits
    
    computationResult =
      compute computationBits computer
  in
  moveProgramCounter jumpBits computationResult  <|
  storeComputationResult destinationsBits computationResult <|
  computer

compute : Int -> Computer -> Int
compute computationBits computer =
  let
    d =
      computer.dRegister
    
    a =
      computer.aRegister

    m =
      computer.mRegister
  in
  case computationBits of
    -- "0101010" ->
    0x2A ->
      0
    
    -- "0111111" ->
    0x3F ->
      1
    
    -- "0111010" ->
    0x3A ->
      -1
    
    -- "0001100" ->
    0xC ->
      d
    
    -- "0110000" ->
    0x30 ->
      a
    
    -- "0001101" ->
    0xD ->
      Bitwise.complement d
    
    -- "0110001" ->
    0x31 ->
      Bitwise.complement a
    
    -- "0001111" ->
    0xF ->
      -d
    
    -- "0110011" ->
    0x33 ->
      -a
    
    -- "0011111" ->
    0x1F ->
      d + 1
    
    -- "0110111" ->
    0x37 ->
      a + 1
    
    -- "0001110" ->
    0xE ->
      d - 1
    
    -- "0110010" ->
    0x32 ->
      a - 1
    
    -- "0000010" ->
    0x2 ->
      d + a

    -- "0010011" ->
    0x13 ->
      d - a
    
    -- "0000111" ->
    0x7 ->
      a - d
    
    -- "0000000" ->
    0x0 ->
      Bitwise.and d a
    
    -- "0010101" ->
    0x15 ->
      Bitwise.or d a
    
    -- "1110000" ->
    0x70 ->
      m

    -- "1110001" ->
    0x71 ->
      Bitwise.complement m
    
    -- "1110011" ->
    0x73 ->
      -m

    -- "1110111" ->
    0x77 ->
      m + 1
    
    -- "1110010" ->
    0x72 ->
      m - 1
    
    -- "1000010" ->
    0x42 ->
      d + m
    
    -- "1010011" ->
    0x53 ->
      d - m
    
    -- "1000111" ->
    0x47 ->
      m - d
    
    -- "1000000" ->
    0x40 ->
      Bitwise.and d m
    
    -- "1010101" ->
    0x55 ->
      Bitwise.or d m
    
    _ ->
      0 -- invalid computation bits


storeComputationResult : Int -> Int -> Computer -> Computer
storeComputationResult destinationsBits result computer =
  let
    storeToARegister =
      getBit 2 destinationsBits
    
    newARegister =
      if storeToARegister then result else computer.aRegister
    
    storeToDRegister =
      getBit 1 destinationsBits
  
    newDRegister =
      if storeToDRegister then result else computer.dRegister

    storeToMRegister =
      getBit 0 destinationsBits
    
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
    , updatedPixels =
      if storeToMRegister then
        if 2 ^ 18 <= computer.aRegister && computer.aRegister <= 2 ^ 18 + 2 ^ 17 // 4 then
          let
            width =
              512 // 4

            offset =
              computer.aRegister - 2 ^ 18
            
            x =
              modBy width offset

            y =
              offset // width
          in
          ((x, y), newMRegister) :: computer.updatedPixels
        else
          computer.updatedPixels
      else
        computer.updatedPixels
  }


moveProgramCounter : Int -> Int -> Computer -> Computer
moveProgramCounter jumpBits computationResult computer =
  let
    zr =
      computationResult == 0
    
    ng =
      computationResult < 0
    
    lt =
      getBit 2 jumpBits
    
    eq =
      getBit 1 jumpBits
    
    gt =
      getBit 0 jumpBits

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


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ editProgramPort EditProgram
    , if model.isRunningComputer then
      Browser.Events.onAnimationFrameDelta StepComputerOneFrame
    else
      Sub.none
    ]


fetchFromMemory : Int -> Memory -> Int
fetchFromMemory address memory =
  Maybe.withDefault 0 <| Array.get address memory


storeToMemory : Int -> Int -> Memory -> Memory
storeToMemory address value memory =
  Array.set address value memory


-- 0th index at the rightmost, least significant bit
getBit : Int -> Int -> Bool
getBit index bits =
  Bitwise.shiftRightBy index bits |>
  Bitwise.and 1 |>
  (\bit -> bit == 1)


dropBits : Int -> Int -> Int
dropBits dropAmount bits =
  Bitwise.shiftLeftBy dropAmount bits |>
  Bitwise.shiftRightZfBy dropAmount


toBinaryString : Int -> String
toBinaryString number =
  String.join "" <| List.map String.fromInt <| Binary.toIntegers <| Binary.fromDecimal number