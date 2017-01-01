module Timstuff.Test where


import qualified Agda.Interaction.CommandLine as CL
import qualified          Agda.TypeChecking.Errors     as ATE
import          qualified Agda.TypeChecking.Monad.Base as MB
import        qualified   Agda.Utils.Impossible        as UI
import        qualified   Control.Monad.Error.Class    as CME
import  qualified         Control.Monad.IO.Class       as CMI
import           GHC.IORef
import    qualified       System.Exit                  as SE
import qualified Agda.Interaction.InteractionTop as IT

import qualified Agda.Interaction.Response as IR
import qualified  Agda.Interaction.Options as IO
import qualified Control.Monad.State as CMS

import qualified Agda.Interaction.EmacsTop as ET
import qualified  Agda.TypeChecking.Monad.Options as MO

import qualified Agda.Interaction.Options as AIO
import qualified Agda.TypeChecking.Monad.State as MS

import qualified Timstuff.ResponseToString as RS

import qualified Agda.Utils.FileName as UF
import qualified  Agda.Interaction.Imports as II

-- | Run a TCM action in IO; catch and pretty print errors.
runTCMPrettyErrors :: MB.TCM a -> IO ()
runTCMPrettyErrors tcm = do
    r <- MB.runTCMTop $ tcm `CME.catchError` \err -> do
      s <- ATE.prettyError err
      CMI.liftIO $ putStrLn s
      CME.throwError err
    case r of
      Right _ -> SE.exitSuccess
      Left _  -> SE.exitFailure
  `UI.catchImpossible` \e -> do
    putStr $ show e
    SE.exitFailure



parseAndPrint :: String -> MB.TCM ()
parseAndPrint x =
              do
                CMI.liftIO $ print "start"
                v <- CL.parseExpr x
                CMI.liftIO $ print v

pap :: String -> IO ()
pap = runTCMPrettyErrors . parseAndPrint



-- clTest :: MB.TCM ()
-- clTest = do
--          CMS.liftIO $ print "Hi there"
--          CMS.runStateT $
--                 (IT.cmd_load' "/home/tim/projects/agda/src/full/Timstuff/test.agda" [] False
--                       $ \(i,mw) -> do
--                                    CMI.liftIO $ print "hi"
--                                    CMI.liftIO $ print $ show i
--                                    IT.display_info IR.Info_CompilationOk)
--                 IT.initCommandState

--          CMS.liftIO $ print "done"
       

mimicTest :: IO ()
mimicTest = runTCMPrettyErrors $ ET.mimicGHCi (CMI.liftIO $ print "hi")

runInteractionTest :: IO ()
runInteractionTest = runTCMPrettyErrors $
                        do
                          MS.setInteractionOutputCallback $
                            CMS.liftIO . mapM_ print CMS.<=< RS.lispifyResponse
                          opts <- MO.commandLineOptions
                          CMS.runStateT doit 
                                               $ IT.initCommandState
                                                  { IT.optionsOnReload = opts{ AIO.optAbsoluteIncludePaths = [] } }
                          return ()
                     where doit = do
                             IT.runInteraction (IT.IOTCM "test.agda" MB.None MB.Indirect
                                                ( IT.Cmd_load "test.agda" [] ))



cmd_loadTest :: IO ()
cmd_loadTest = runTCMPrettyErrors $
                        do
                          MS.setInteractionOutputCallback $
                            CMS.liftIO . mapM_ print CMS.<=< RS.lispifyResponse
                          opts <- MO.commandLineOptions
                          CMS.runStateT doit 
                                               $ IT.initCommandState
                                                  { IT.optionsOnReload = opts{ AIO.optAbsoluteIncludePaths = [] } }
                          return ()
                     where doit = do
                             IT.cmd_load' "test.agda" [] True (\_ ->
                                                                do
                                                                        IT.displayStatus
                                                                        IT.displayStatus
                                                              )



typeCheckMainTest :: IO ()
typeCheckMainTest = runTCMPrettyErrors $
                       do
                         MS.setInteractionOutputCallback $
                            CMS.liftIO . mapM_ print CMS.<=< RS.lispifyResponse
                         opts <- MO.commandLineOptions
                         MO.setCommandLineOptions opts
                         II.typeCheckMain (UF.mkAbsolute "/home/tim/projects/agda/test.agda")
                         return ()
                         
                         
