# binomial

Calculate option values uses the binomial pricing model.

## Usage

git clone git@github.com:vonrosen/binomial.git

lein uberjar

lein run <call_option_or_not> <underlying_price> <strike_price> <risk free rate of return> <time period for discounting> <up movement factor> <down movement factor> <number of levels in binomial tree not including root>

## Example
  
call option, 100 underlying price, 110 strike price, 10% risk free rate of return, discount over half the period of risk free rate of return, 20% up move percent of underlying per time period, 20% down move percent of underlying per time period

```lein run true 100 110 .1 .5 1.2 .8 2```

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
