module LiveLines.Api where

import Servant
import LiveLines.Context
import Lines.Api

type LiveLinesApi = LinesApi

liveLinesServer :: ServerT LiveLinesApi AppM
liveLinesServer = linesHandler