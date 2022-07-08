## Ada Price Fetcher
The `ada-price-fetcher` is a script that fetches the current Ada price in USD (or a USD stable coin) and computes the unbiased median.
It is possible to provide a minimal amount of price-data that needs to be fetched successfully for the script to succeed. It can be set using the option `--min-prices <number>` or `-N <number>`. The default value is 5.

The following table gives an overview over the sources being used.
Unfortunately the different sources don't provide their prices in the same currency and their types of prices also differ. But investigations have shown that the different price types don't differ significantly.

| Source   | API Endpoint | Price Currency | Price Type | 
|----------|--------------|---|---|
| Binance  | [Symbol Price Ticker](https://binance-docs.github.io/apidocs/spot/en/#symbol-price-ticker) | USDT (USD not available) | No information |  
| Coinbase | [Get Spot Price](https://docs.cloud.coinbase.com/sign-in-with-coinbase/docs/api-prices#get-spot-price) | USD | spot |  
| Huobi    | [Market Data](https://huobiapi.github.io/docs/spot/v1/en/#get-latest-aggregated-ticker) | USDT (USD not available) | ask + bid / 2 ~ spot |
| Kraken   | [Get Ticker Information](https://docs.kraken.com/rest/#operation/getTickerInformation) | USD |  volume weighted average |
| Kucoin   | [Get Fiat Price](https://docs.kucoin.com/#get-fiat-price) | USD | No information |