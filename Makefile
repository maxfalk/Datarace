##############
## Git help
##############

PULL := git stash && git pull && git stash apply

PUSH := git add -A && git commit -m "$(COMMENT)" && $(PULL) && git push

#############

pull: 
	$(PULL)

push: 
	$(PUSH)
