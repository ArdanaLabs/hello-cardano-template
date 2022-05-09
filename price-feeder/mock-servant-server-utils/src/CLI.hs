module CLI where

import Options.Applicative

data Serve = Serve {
  _priceDataPath:: FilePath,
  _port :: Int
}

mockServerOptions :: String -> ParserInfo Serve
mockServerOptions name = info (serve <**> helper)
                          (fullDesc
                        <> progDesc ("Mocks the " <> name <> " service")
                        <> header (name <> "-server-mock") )

serve :: Parser Serve
serve = Serve <$> priceDataPath <*> port

priceDataPath :: Parser FilePath
priceDataPath = argument str
               ( metavar "JSONFILE" )
port :: Parser Int
port = argument auto
         ( metavar "PORT" )