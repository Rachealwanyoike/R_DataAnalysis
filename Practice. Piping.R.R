foo_foo <- little bunny()
hop <- hop()
scoop <- scoop()
bop <- bop()


foo_foo_1 <- hop(foo_foo, through = forest)
foo_foo_2 <- scoop(foo_foo_1, up = field_mice)
foo_foo_3 <- bop(foo_foo_2, on = head)


gapminder%>%
  select(Country, lifeexp, gdppercapita)%>%
  filter(country=="kenya")
