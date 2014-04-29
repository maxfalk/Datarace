##############
## Git help
##############

PULL := git stash && git pull && git stash apply
PUSH := git add -A && git commit && git push

#############

pull: 
	$(PULL)

push: 
	$(PULL) $(PUSH)
