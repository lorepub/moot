{-# OPTIONS_GHC -fno-warn-orphans #-}

module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic

/           HomeR    GET

-- Auth
/login      LoginR   GET POST
/signup     SignupR  GET POST
/signout    SignoutR GET

-- Admin
/organizer/signup OrganizerSignupR GET POST
/conference/#Int64/abstract-types ConferenceAbstractTypesR GET POST

/cfp/submit SubmitAbstractR GET POST
|]
