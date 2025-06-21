import * as S from "sury";

export const databaseContextSchema = S.schema({
  schemas: S.nullable(
    S.array(
      S.schema({
        schema: S.string,
        tables: S.nullable(
          S.array(
            S.schema({
              name: S.string,
              columns: S.nullable(
                S.array(
                  S.schema({
                    name: S.string,
                    type: S.string,
                    nullable: S.boolean,
                    default: S.nullable(S.string),
                  })
                ),
                []
              ),
            })
          ),
          []
        ),
      })
    ),
    []
  ),
  enums: S.nullable(
    S.array(
      S.schema({
        schema: S.string,
        name: S.string,
        value: S.string,
      })
    ),
    []
  ),
});
