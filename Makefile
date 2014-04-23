##############
## Git help
##############

PULL = $(git stash && git pull && git stash apply)
PUSH = $(git add -A )

#############

pull: $(PULL)

push:
