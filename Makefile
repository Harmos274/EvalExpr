BINARY_PATH 	:=	$(shell stack path --local-install-root)
NAME 			= 	funEvalExpr

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME) ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
