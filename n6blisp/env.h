#ifndef ENV_H

OBJ env_null();
OBJ env_init();
OBJ extend_environment(OBJ names,OBJ values,OBJ env);
OBJ lookup_variable_value(OBJ name,OBJ env);
OBJ lookup_variable_cell(OBJ name,OBJ env);
void set_variable_value(OBJ name,OBJ value,OBJ env);
OBJ define(OBJ name,OBJ value,OBJ env);
OBJ make_env(OBJ base,OBJ frame);

#endif
