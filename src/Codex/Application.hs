{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-} -- Needed by
{-# LANGUAGE TypeFamilies      #-} -- web-routes
{-# LANGUAGE DeriveGeneric     #-} -- Needed to derive Generic
                                   -- for our URL data type

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Codex.Application where

------------------------------------------------------------------------------
import Control.Lens

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.SqliteSimple
import Snap.Snaplet.Router.Types

import System.FastLogger (Logger)

import Control.Monad.State (get)

import Data.Configurator.Types (Config)

import Codex.Tasks
import Codex.Types
import Codex.Handlers
import Codex.Tester.Monad
import Codex.Tester.Result


-- | URLs for our application
data AppUrl =
    Admin                  -- ^ admin operations page
  | Login                  -- ^ session login / logout
  | Logout
  | Register
  | Page [FilePath]        -- ^ exercise page or other file
  | Report SubmitId        -- ^ report for previous submission
  | Shibboleth             -- ^ shibboleth sso login
  -- following are for adminstrator only
  | Files [FilePath]          -- ^ file browser 
  | SubmissionList            -- ^ submissions list
  | SubmissionAdmin SubmitId  -- ^ a single submission 
    deriving (Eq, Show, Read, Generic)

instance PathInfo AppUrl

------------------------------------------------------------------------------
type Codex = Handler App App
------------------------------------------------------------------------------

data App = App
    { _heist   :: Snaplet (Heist App)
    , _router  :: Snaplet RouterState
    , _sess    :: Snaplet SessionManager
    , _auth    :: Snaplet (AuthManager App)
    , _db      :: Snaplet Sqlite
    , _tester  :: Tester Result    -- ^ exercise testers to use
    , _handlers :: Handlers Codex  -- ^ exercise handlers to use
    , _taskGroup :: TaskGroup      -- ^ task group for controlling concurrency
    , _queue   :: Queue            -- ^ queue for pending re-evaluations
    , _logger  :: Logger
    , _eventcfg :: Config          -- ^ events configuration
   }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasSqlite (Handler App App) where
   getSqliteState = with db get



-- You need to define a HasRouter instance for your app.
-- You must set type URL (Handler App App) to the URL
-- data type you defined above. The router in
-- `with router` is the lens for the @RouterState@ snaplet
-- you added to App.
instance HasRouter (Handler App App) where
    type URL (Handler App App) = AppUrl
    getRouterState = with router get
    

-- You also need to define a HasRouter instance for the
-- router snaplet. Once again, set type URL (Handler b
-- RouterState) to the data type you defined above.
instance HasRouter (Handler b RouterState) where
    type URL (Handler b RouterState) = AppUrl
    getRouterState = get
    

