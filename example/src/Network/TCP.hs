{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
module Network.TCP where

import           Control.Algebra.Free
import           Control.Monad.Free (Free (..))
import           Control.Exception (IOException, try)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Data.Bifunctor (bimap, first)
import           Data.ByteString.Lazy as LBS
import           Data.Binary (Binary (..), encode, decodeOrFail)
import qualified Data.Binary as Binary
import           Data.Functor (($>))
import           Data.Int (Int64)

import           Network.Socket (Socket)
import           Network.Socket.ByteString.Lazy (send, recv)


data TransportException
    = IOTransportException IOException
    -- | todo add tcp level errors
    | TCPProtocolError String
    | DecoderError String

-- |
-- Transport layer DSL
data Transport_F msg a
    = Send msg a
    | Recv Int64 (Either TransportException msg -> a)
    deriving (Functor)

-- |
-- Run transport layer in @'IO'@
runTransportIO
    :: forall msg a. Binary msg
    => Socket
    -> Free (Transport_F msg) a
    -> IO a
runTransportIO sock = foldMapFree1 f
    where
    f :: Transport_F msg x -> IO x
    f (Send msg a) =
        send sock (encode msg) $> a
    f (Recv max cont) = do
        ebs <- try $ recv sock max
        case ebs of
            Left ioerr -> return $ cont (Left (IOTransportException ioerr))
            Right bs   ->
                let msg = bimap
                        (\(_, _, err) -> DecoderError err)
                        (\(_, _, a)   -> a)
                        (decodeOrFail bs)
                in return (cont msg)

-- |
-- Tcp messages
data TcpMsg a
    = Syn
    | SynAck
    | Ack
    | Fin
    | FinAck1
    | FinAck2
    | TcpMsg a
    | TcpMsgAck
    deriving (Show, Eq, Ord)

instance Binary a => Binary (TcpMsg a) where
    put Syn     = Binary.putWord8 0
    put SynAck  = Binary.putWord8 1
    put Ack     = Binary.putWord8 2
    put Fin     = Binary.putWord8 3
    put FinAck1 = Binary.putWord8 4
    put FinAck2 = Binary.putWord8 5
    put (TcpMsg m) = do
        Binary.putWord8 6
        put m
    put TcpMsgAck = Binary.putWord8 7
    get = do 
        x <- Binary.getWord8
        case x of
            0 -> return Syn
            1 -> return SynAck
            2 -> return Ack
            3 -> return Fin
            4 -> return FinAck1
            5 -> return FinAck2
            6 -> TcpMsg <$> get
            7 -> return TcpMsgAck
            _ -> fail "TcpMsg: unknown tag"

-- |
-- TCP layer DSL
data TCP_F msg a
    = Handshake (Maybe TransportException -> a)
    | SendMsg msg a
    | RecvMsg Int64 (Either TransportException (TcpMsg msg) -> a)
    | CloseConnection (Maybe TransportException -> a)
    deriving Functor

-- |
-- Interpret the @'TCP_F'@ functor in the free monad @'Free' 'Transport_F'@.
interpTCP
    :: (Binary msg, Show msg)
    => TCP_F msg a
    -> Free (Transport_F (TcpMsg msg)) a
interpTCP (Handshake cont) = do
    returnFree1 (Send Syn ())
    merr <- returnFree1 $ Recv 1 $ \msg ->
        case msg of
            Left e
                -> Just e
            Right SynAck
                -> Nothing
            Right msg
                -> Just $ TCPProtocolError ("Expected SynAck message but received: " ++ show msg)
    case merr of
        Nothing  -> returnFree1 (Send SynAck $ cont Nothing)
        Just err -> return (cont $ Just err)
interpTCP (SendMsg msg a) =
    returnFree1 (Send (TcpMsg msg) a)
interpTCP (CloseConnection cont) = do
    returnFree1 (Send Fin ())
    merr <- runExceptT $ do
        ExceptT $ returnFree1 $ Recv 1 $ \msg ->
            case msg of
                Left e
                    -> Left e
                Right FinAck1
                    -> Right ()
                Right msg
                    -> Left $ TCPProtocolError ("Expected FinAck message but received: " ++ show msg)
        ExceptT $ returnFree1 $ Recv 1 $ \msg ->
            case msg of
                Left e
                    -> Left e
                Right Fin
                    -> Right ()
                Right msg
                    -> Left $ TCPProtocolError ("Expected Fin message but received: " ++ show msg)
        ExceptT $ returnFree1 $ Send FinAck2 (Right ())
    case merr of
        Left err -> return $ cont (Just err)
        Right () -> return $ cont Nothing
interpTCP (RecvMsg max cont) = do
    msg <- returnFree1 $ Recv max id
    case msg of
        Left err  -> return (cont $ Left err)
        Right Syn -> returnFree1 (Send SynAck (cont $ Right Syn))
        Right SynAck
                  -> returnFree1 (Send Ack (cont $ Right SynAck))
        Right Fin -> returnFree1 (Send FinAck1 (cont $ Right Fin))
        Right FinAck1
                  -> return (cont $ Right FinAck1)
        Right FinAck2
                  -> return (cont $ Right FinAck2)
        Right (TcpMsg msg)
                  -> return (cont $ Right (TcpMsg msg))
        Right TcpMsgAck
                  -> return (cont $ Right TcpMsgAck)

-- |
-- Use @'bindFree1'@ to run @'Free' 'TCP_F'@ monad in @'Free' 'Transport_F'@
-- monad.
runTCP
    :: (Binary msg, Show msg)
    => Free (TCP_F msg) a
    -> Free (Transport_F (TcpMsg msg)) a
runTCP tcp = tcp `bindFree1` interpTCP

-- |
-- Run @'Free 'TCP_F'@ monad in @'IO'@
runTCP_IO
    :: forall msg a. (Binary msg, Show msg)
    => Socket
    -> Free (TCP_F msg) a
    -> IO a
runTCP_IO sock = runTransportIO @(TcpMsg msg) sock . runTCP

-- |
-- Application level messges.
data Message = Hello | Bye
    deriving (Show, Eq, Ord)

instance Binary Message where
    put Hello = Binary.putWord8 0
    put Bye   = Binary.putWord8 1
    get = do
        x <- Binary.getWord8
        case x of
            0 -> return Hello
            1 -> return Bye
            _ -> fail "Message: unknown tag"
