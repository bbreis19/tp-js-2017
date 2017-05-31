module Main exposing (..)

import Html            exposing (Html, h1, button, div, text, form, input, textarea, label, fieldset, p)
import Html.Attributes exposing (class, id, style, name, rows, for, type_, method, required)
import Html.Events     exposing (onInput, onSubmit)
import Json.Encode     exposing (string, list, object)
import Json.Decode     exposing (succeed)
import Http            exposing (Error, post, send, Body, jsonBody)

-- Ações, em si, usadas pelo Update.
type Msg
    = Nome      String
    | Sobrenome String
    | Email     String
    | Telefone  String
    | Mensagem  String
    | Enviar
    | Resposta  (Result Error ())

-- Model, onde fica o "estado" da applicação.
type alias Model =
    { nome      : String
    , sobrenome : String
    , email     : String
    , telefone  : String
    , mensagem  : String
    , estado    : String
    }

-- Converte os dados do model para um JSON válido.
encodeModel : Model -> Body
encodeModel m = jsonBody <| object
    [("mailto", object
        [ ("nome"     , string m.nome)
        , ("sobrenome", string m.sobrenome)
        , ("email"    , string m.email)
        , ("telefone" , string m.telefone)
        , ("mensagem" , string m.mensagem)
        ]
    )]
    
-- Update, o que fica ouvindo por ações (como um addEventListener) vindas da view ou do próprio update e comete ações de acordo com a Msg enviada.
update : Msg -> Model -> (Model, Cmd Msg) -- Cmd representa ações lançadas pela própria aplicação.
update msg m = case msg of
    Nome      x -> ({m | nome      = x}, Cmd.none)
    Sobrenome x -> ({m | sobrenome = x}, Cmd.none)
    Email     x -> ({m | email     = x}, Cmd.none)
    Telefone  x -> ({m | telefone  = x}, Cmd.none)
    Mensagem  x -> ({m | mensagem  = x}, Cmd.none)
    
    Enviar -> (m, send Resposta <| post "https://httpbin.org/post" (encodeModel m) (succeed ())) -- URL aleatória, apenas para testes.
    
    Resposta (Ok _)  -> ({m | estado = "Enviado"} , Cmd.none)
    Resposta (Err e) -> ({m | estado = toString e}, Cmd.none)

-- View, a parte do HTML e onde as ações, para o Update, são penduradas.
view : Model -> Html Msg
view m = div [class "contato"]
    [ h1 [] [ text "Contato" ]
    , div [class "formulario"]
        [ form [id "submit", onSubmit Enviar, method "post"]
            [ fieldset []
                [ p [] [ text m.estado ]
                , fieldset [class "grupo"]
                    [ div [class "campo"]
                        [ label [for "nome"] [ text "Nome" ]
                        , input [onInput Nome, type_ "text", id "nome", name "nome", style [("width", "10em")], required True] []
                        ]
                    , div [class "campo"]
                        [ label [for "snome"] [ text "Sobrenome" ]
                        , input [onInput Sobrenome, type_ "text", id "snome", name "snome", style [("width", "10em")], required True] []
                        ]
                    ]
                , div [class "campo"]
                    [ label [for "email"] [ text "E-mail" ]
                    , input [onInput Email, type_ "email", id "email", name "email", style [("width", "20em")], required True] []
                    ]
                , div [class "campo"]
                    [ label [for "telefone"] [ text "Telefone" ]
                    , input [onInput Telefone, type_ "tel", id "telefone", name "telefone", style [("width", "10em")], required True] []
                    ]
                , div [class "campo"]
                    [ label [for "mensagem"] [ text "Mensagem" ]
                    , textarea [onInput Mensagem, rows 6, id "snome", name "mensagem", style [("width", "20em")], required True] []
                    ]
                , button [class "button"] [ text "ENVIAR" ]
                ]
            ]
        ]
    ]

-- Entrada, valores iniciais.
main = Html.program
    { init = (
        { nome      = "" 
        , sobrenome = ""
        , email     = ""
        , telefone  = ""
        , mensagem  = ""
        , estado    = ""
        }, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }