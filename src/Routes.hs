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
/contact    ContactR GET
/forgot     ForgotR  GET POST
/reset/#Token  ResetR   GET POST

-- Admin
/organizer/signup                 OrganizerSignupR GET POST
/conferences                      ConferencesR GET
/conference/#Int64                ConferenceDashboardR GET
/conference/#Int64/abstract-types ConferenceAbstractTypesR GET POST

/cfp/submit SubmitAbstractR GET POST
|]
