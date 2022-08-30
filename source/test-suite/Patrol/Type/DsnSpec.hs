module Patrol.Type.DsnSpec where

import qualified Data.Text as Text
import qualified Network.URI as Uri
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
      uri <- parseUri "invalid:"
      Dsn.fromUri uri `Hspec.shouldBe` Nothing

    Hspec.it "succeeds with a minimal DSN" $ do
      uri <- parseUri "a://b@c/d"
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
      Dsn.fromUri uri `Hspec.shouldBe` Just dsn

    Hspec.it "succeeds with a maximal DSN" $ do
      uri <- parseUri "a://b:c@d:5/f/g"
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
      Dsn.fromUri uri `Hspec.shouldBe` Just dsn

    Hspec.it "fails with a query" $ do
      uri <- parseUri "a://b@c/d?"
      Dsn.fromUri uri `Hspec.shouldBe` Nothing

    Hspec.it "fails with a fragment" $ do
      uri <- parseUri "a://b@c/d#"
      Dsn.fromUri uri `Hspec.shouldBe` Nothing

  Hspec.describe "toUri" $ do
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
      uri <- parseUri "a://b@c/d"
      Dsn.toUri dsn `Hspec.shouldBe` uri

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
      uri <- parseUri "a://b:c@d:5/f/g"
      Dsn.toUri dsn `Hspec.shouldBe` uri

parseUri :: String -> IO Uri.URI
parseUri string = case Uri.parseURI string of
  Nothing -> fail $ "invalid URI: " <> show string
  Just uri -> pure uri
