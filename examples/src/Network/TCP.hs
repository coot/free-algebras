{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs          #-}
{- An example implementation of TCP protocol over Send / Receive primitives.
 - There are two layers: first DSL can only know how to send and receive
 - messagse (through a socket).  On top of this there is a TCP DSL which can
 - establish connection, send messages, handles ACKs and tear the connection
 - down.
 -
 -  The first DSL is desribed by a functor `Transport_F`.  It also has an
 -  interpration in `IO`, via `runTransportIO` function.
 -
 -  The second DSL is defined by a functor `TCP_F` and we interpret it in `m
 -  Transport_F` where `m` satisifies `FreeAlgebra1 m` constraint (e.g.
 -  a @'Free'@ monad).
 -
 -  @'runTCP'@ and @'runTCP_IO'@ let you interpret the tcp dsl in
 -  @'FreeAlgebra1' m => m 'Transport_F'@ (e.g. @'Free' 'Transport_F'@ monad)
 -  and @'IO'@.
 -}
module Network.TCP where

import           Control.Algebra.Free
import           Control.Exception (IOException, try)
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Data.Bifunctor (bimap)
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
-- Run transport layer in @'IO'@, @m@  is any `FreeAlgebra1 m` for which @IO@
-- has the same type.  This allows you to plug any `Free` monad as @m@.
runTransportIO
    :: forall msg a m.
       ( Binary msg
       , FreeAlgebra1 m
       , AlgebraType  m IO
       , AlgebraType0 m (Transport_F msg)
       )
    => Socket
    -> m (Transport_F msg) a
    -> IO a
runTransportIO sock = foldNatFree nat
    where
    nat :: Transport_F msg x -> IO x
    nat (Send msg a) =
        send sock (encode msg) $> a
    nat (Recv len cont) = do
        ebs <- try $ recv sock len
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
-- Interpret the @'TCP_F'@ functor in @'Free' 'Transport_F'@ (or any other free monad).
interpTCP
    :: ( FreeAlgebra1 m
       , AlgebraType0 m (Transport_F (TcpMsg msg))
       , Monad (m (Transport_F (TcpMsg msg)))
       , Show msg
       )
    => TCP_F msg a
    -> m (Transport_F (TcpMsg msg)) a
interpTCP (Handshake cont) = do
    liftFree (Send Syn ())
    merr <- liftFree $ Recv 1 $ \mmsg ->
        case mmsg of
            Left e
                -> Just e
            Right SynAck
                -> Nothing
            Right msg
                -> Just $ TCPProtocolError ("Expected SynAck message but received: " ++ show msg)
    case merr of
        Nothing  -> liftFree (Send SynAck $ cont Nothing)
        Just err -> return (cont $ Just err)
interpTCP (SendMsg msg a) =
    liftFree (Send (TcpMsg msg) a)
interpTCP (CloseConnection cont) = do
    liftFree (Send Fin ())
    merr <- runExceptT $ do
        ExceptT $ liftFree $ Recv 1 $ \mmsg ->
            case mmsg of
                Left e
                    -> Left e
                Right FinAck1
                    -> Right ()
                Right msg
                    -> Left $ TCPProtocolError ("Expected FinAck message but received: " ++ show msg)
        ExceptT $ liftFree $ Recv 1 $ \mmsg ->
            case mmsg of
                Left e
                    -> Left e
                Right Fin
                    -> Right ()
                Right msg
                    -> Left $ TCPProtocolError ("Expected Fin message but received: " ++ show msg)
        ExceptT $ liftFree $ Send FinAck2 (Right ())
    case merr of
        Left err -> return $ cont (Just err)
        Right () -> return $ cont Nothing
interpTCP (RecvMsg len cont) = do
    mmsg <- liftFree $ Recv len id
    case mmsg of
        Left err  -> return (cont $ Left err)
        Right Syn -> liftFree (Send SynAck (cont $ Right Syn))
        Right SynAck
                  -> liftFree (Send Ack (cont $ Right SynAck))
        Right Ack -> return (cont $ Right Ack)
        Right Fin -> liftFree (Send FinAck1 (cont $ Right Fin))
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
    :: ( FreeAlgebra1 m
       , AlgebraType0 m (Transport_F (TcpMsg msg))
       , AlgebraType0 m (TCP_F msg)
       , Monad (m (Transport_F (TcpMsg msg)))
       , Show msg
       )
    => m (TCP_F msg) a
    -> m (Transport_F (TcpMsg msg)) a
runTCP tcp = tcp `bindFree1` interpTCP

-- |
-- Run @'Free' 'TCP_F'@ monad in @'IO'@ (or any other free monad).
runTCP_IO
    :: forall msg m a.
       ( FreeAlgebra1 m
       , AlgebraType0 m (Transport_F (TcpMsg msg))
       , AlgebraType0 m (TCP_F msg)
       , AlgebraType  m IO
       , Monad (m (Transport_F (TcpMsg msg)))
       , Binary msg
       , Show msg
       )
    => Socket
    -> m (TCP_F msg) a
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
