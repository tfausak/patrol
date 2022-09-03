{-# LANGUAGE QuasiQuotes #-}

module Patrol.Type.DsnSpec where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.URI.Static as Uri
import qualified Patrol.Type.Dsn as Dsn
import qualified Patrol.Type.Host as Host
import qualified Patrol.Type.Path as Path
import qualified Patrol.Type.Port as Port
import qualified Patrol.Type.ProjectId as ProjectId
import qualified Patrol.Type.Protocol as Protocol
import qualified Patrol.Type.PublicKey as PublicKey
import qualified Patrol.Type.SecretKey as SecretKey
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Patrol.Type.Dsn" $ do
  Hspec.describe "fromUri" $ do
    Hspec.it "fails with an invalid DSN" $ do
      Dsn.fromUri [Uri.uri|a:|] `Hspec.shouldBe` Nothing

    Hspec.it "succeeds with a minimal DSN" $ do
      let dsn =
            Dsn.Dsn
              { Dsn.protocol = Protocol.Protocol $ Text.singleton 'a',
                Dsn.publicKey = PublicKey.PublicKey $ Text.singleton 'b',
                Dsn.secretKey = Nothing,
                Dsn.host = Host.Host $ Text.singleton 'c',
                Dsn.port = Nothing,
                Dsn.path = Path.Path $ Text.singleton '/',
                Dsn.projectId = ProjectId.ProjectId $ Text.singleton 'd'
              }
      Dsn.fromUri [Uri.uri|a://b@c/d|] `Hspec.shouldBe` Just dsn

    Hspec.it "succeeds with a maximal DSN" $ do
      let dsn =
            Dsn.Dsn
              { Dsn.protocol = Protocol.Protocol $ Text.singleton 'a',
                Dsn.publicKey = PublicKey.PublicKey $ Text.singleton 'b',
                Dsn.secretKey = Just . SecretKey.SecretKey $ Text.singleton 'c',
                Dsn.host = Host.Host $ Text.singleton 'd',
                Dsn.port = Just $ Port.Port 5,
                Dsn.path = Path.Path $ Text.pack "/f/",
                Dsn.projectId = ProjectId.ProjectId $ Text.singleton 'g'
              }
      Dsn.fromUri [Uri.uri|a://b:c@d:5/f/g|] `Hspec.shouldBe` Just dsn

    Hspec.it "fails with a query" $ do
      Dsn.fromUri [Uri.uri|a://b@c/d?|] `Hspec.shouldBe` Nothing

    Hspec.it "fails with a fragment" $ do
      Dsn.fromUri [Uri.uri|a://b@c/d#|] `Hspec.shouldBe` Nothing

  Hspec.describe "intoUri" $ do
    Hspec.it "converts a minimal DSN into URI" $ do
      let dsn =
            Dsn.Dsn
              { Dsn.protocol = Protocol.Protocol $ Text.singleton 'a',
                Dsn.publicKey = PublicKey.PublicKey $ Text.singleton 'b',
                Dsn.secretKey = Nothing,
                Dsn.host = Host.Host $ Text.singleton 'c',
                Dsn.port = Nothing,
                Dsn.path = Path.Path $ Text.singleton '/',
                Dsn.projectId = ProjectId.ProjectId $ Text.singleton 'd'
              }
      Dsn.intoUri dsn `Hspec.shouldBe` [Uri.uri|a://b@c/d|]

    Hspec.it "converts a maximal DSN into URI" $ do
      let dsn =
            Dsn.Dsn
              { Dsn.protocol = Protocol.Protocol $ Text.singleton 'a',
                Dsn.publicKey = PublicKey.PublicKey $ Text.singleton 'b',
                Dsn.secretKey = Just . SecretKey.SecretKey $ Text.singleton 'c',
                Dsn.host = Host.Host $ Text.singleton 'd',
                Dsn.port = Just $ Port.Port 5,
                Dsn.path = Path.Path $ Text.pack "/f/",
                Dsn.projectId = ProjectId.ProjectId $ Text.singleton 'g'
              }
      Dsn.intoUri dsn `Hspec.shouldBe` [Uri.uri|a://b:c@d:5/f/g|]

  Hspec.describe "intoAuthorization" $ do
    Hspec.it "works without a secret key" $ do
      dsn <- maybe (fail "invalid DSN") pure $ Dsn.fromUri [Uri.uri|a://b@c/d|]
      let byteString = Text.encodeUtf8 $ Text.pack "Sentry sentry_version=7,sentry_key=b"
      Dsn.intoAuthorization dsn `Hspec.shouldBe` byteString

    Hspec.it "works with a secret key" $ do
      dsn <- maybe (fail "invalid DSN") pure $ Dsn.fromUri [Uri.uri|a://b:c@d/e|]
      let byteString = Text.encodeUtf8 $ Text.pack "Sentry sentry_version=7,sentry_key=b,sentry_secret=c"
      Dsn.intoAuthorization dsn `Hspec.shouldBe` byteString
