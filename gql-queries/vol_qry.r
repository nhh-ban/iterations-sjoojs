
# GQL for volumes, task 4b
vol_qry <- function(id, from, to) {
  query <- sprintf('{
    trafficData(trafficRegistrationPointId: "%s") {
      volume {
        byHour(from: "%s", to: "%s") {
          edges {
            node {
              from
              to
              total {
                volumeNumbers {
                  volume
                }
              }
            }
          }
        }
      }
    }
  }', id, from, to)
  return(query)
}

# testing
GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)


