#!/usr/bin/env python3

import argparse
import sys

# cagrs as a fraction
def CAGR(initial, end, n_years):
    # print(f"calculating cagr: initial={initial}, end={end}, n_years={n_years}")
    return (end / initial)**(1 / n_years) - 1

def cagr_test():
    c1 = CAGR(100, 200, 1)
    c2 = CAGR(1000.0, 1980, 6)
    print(f"cagr test: {c1:.2%}")
    print(f"cagr test: {c2:.2%}")
    c3 = CAGR(863352.00, 863352.00 + 720000.00, 1)
    print(f"cagr test: {c3:.2%}")

'''
Notes:
- Mortage conf is assumed to be calculated elsewhere, here we just take monthly
  payment, years and loan size as input. Defaults are for 12m for 30 years at 6%
  interest.
- Inflation is used to calculate mortage payment reduction over time. Default 6% is 
  conservative I'd say.
- Realty price is assumed to grow exactly with inflation, so we assume no 
  profit/loss here and so no calculations are done. Which is also conservative.
- Rental is assumed to grow with exactly inflation too, so we don't do anything here.
- Amortization, taxes etc are assumed to be included into rental.
'''
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="""
                                     calculate some vovsu donimaet
                                     """)
    parser.add_argument("-y", help="years", type=int)
    parser.add_argument("--mortage-payment", help="monthly mortage payment", type=int, default=71946)
    parser.add_argument("-r", help="monthly rental", type=int)
    parser.add_argument("-i", help="inflation in %", type=float, default=6.0)
    parser.add_argument("-s", help="how many years to skip before rental starts", type=int, default=3)
    parser.add_argument("--price", help="realty price, used to calculate down payment", type=int, default=15000000)
    parser.add_argument("-l", help="loan size, part of price", type=int, default=12000000)
    parser.add_argument("--renovation", help="renovation cost. 50k per 1m2 seems reasonable. Unlike down payment, not assumed to be returned when sold, which is not clear actually", type=int, default=2000000)
    args = parser.parse_args()
    print(args.y)

    # cagr_test()
    # sys.exit(0)
    
    years = args.y
    payment = args.mortage_payment
    rental = args.r
    inflation_f = args.i / 100.0
    skip_years = args.s
    price = args.price
    loan = args.l
    # initial
    down_payment = price - loan
    renovation = args.renovation
    print(f"inflation factor: {inflation_f:.4f}, skip years: {skip_years}, loan: {loan}, down_payment: {down_payment}, renovation: {renovation}")

    payment_inf_adjusted = payment
    # sum of loan payed out
    payment_sum_inf_adjusted = 0
    payment_sum = 0
    # sum of investments to cover payment while rental is not enough
    payment_investments = 0
    rental_sum = 0
    # part of rental left after covering payments
    rental_profit = 0
    for i in range(0, years):
        payment_yearly_inf_adjusted = payment_inf_adjusted * 12
        payment_sum_inf_adjusted += payment_yearly_inf_adjusted
        payment_sum += payment * 12

        # Pure this year rental income not broke up into payment and profit parts
        if i >= skip_years:
            yearly_rental = rental * 12
        else:
            yearly_rental = 0
        # Pure rental income not broke up into payment and profit parts
        rental_sum += yearly_rental

        # Break rental into payment and profit because putting it into payment reduces investments.
        if yearly_rental <= payment_yearly_inf_adjusted:
            # all rental goes to payment + we need investments to cover the rest
            payment_investments += payment_yearly_inf_adjusted - yearly_rental
        else:
            # rental covers payment, the rest is profit
            rental_profit += yearly_rental - payment_yearly_inf_adjusted

        # total (cumulative) rental minus payment balance
        balance = rental_sum - payment_sum_inf_adjusted
        # current year rental minus payment balance
        yearly_balance = yearly_rental - payment_yearly_inf_adjusted
        # takes into account renovation but not down payment because the latter is assumed to be returned when selling
        full_balance = balance - args.renovation

        investments = down_payment + renovation + payment_investments
        assets = payment_inf_adjusted + rental_profit

        # Note that in this basic model with mortage % == inflation % (free loan) and realty price changing exactly with 
        # inflation (buying + selling gives zero), ignoring renovation profit just equals rental_sum.

        # cagr = CAGR(payment_sum_inf_adjusted, payment_sum_inf_adjusted + rental_sum, i + 1)
        #
        # Interesting points:
        # - when payment_inf_adjusted gets to rental, i.e. thing becomes self sustainable
        # - when accumulated the rest of rental - payment_inf_adjusted buys back the initial investments
        # - total (mostly initial) investments size
        print(f"year {i}: monthly payment_i_a={payment_inf_adjusted:.2f}, payment_sum_i_a={payment_sum_inf_adjusted:.2f}, payment_sum={payment_sum:.2f} monthly rental={rental:.2f}, rental_sum={rental_sum:.2f}, yearly b={yearly_balance:.2f}, b={balance:.2f}, full_b={full_balance:.2f}, rental profit={rental_profit:.2f}, investments={investments:.2f}, assets={assets:.2f}")

        payment_inf_adjusted = payment_inf_adjusted * (1 - inflation_f)
        # not changing rental assumes it changes with inflation