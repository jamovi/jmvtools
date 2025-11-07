
# jmvtools

An R package, analagous to devtools, which makes it easy to develop jamovi modules

To install this package, issue the following command from the R terminal:

```{r}
install.packages('jmvtools', repos='https://repo.jamovi.org')
```

### Updating jamovi-compiler

The `jamovi-compiler` directory from the `jamovi/jamovi` needs to be mounted at `inst/node_modules/jamovi-compiler` of this project.

We manage this by maintaining a subtree branch called `compiler` in `jamovi/jamovi`. To update this branch (so that it contains the current contents of the `jamovi-compiler` subdirectory):

```
# 1. repo up-to-date

git checkout main
git pull

# 2. create a temp subtree branch

git subtree split -P jamovi-compiler -b tmp/compiler

# 3. merge it into the compiler branch

git checkout compiler
git merge tmp/compiler
git push

# 4. tidy up (delete the tmp)

git branch -D tmp/compiler
```

Having updated the subtree branch, we now update the subdirectory in the `jmvtools` repo.

```
# 1. Add jamovi/jamovi remote (only necessary the first time)

git remote add jamovi https://github.com/jamovi/jamovi.git

# 2.

git fetch jamovi
git subtree pull --prefix=inst/node_modules/jamovi-compiler jamovi compiler --squash

```
